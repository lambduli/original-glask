module Compiler.TypeSystem.Type.Infer.Program where

import qualified Data.Map.Strict as Map
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity
import Control.Monad.State


import Compiler.Counter

import Compiler.Syntax

import Compiler.TypeSystem.Error
import Compiler.TypeSystem.Program
import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.InferenceState
import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Type.Infer.BindSection
import Compiler.TypeSystem.Type.Infer.Method
import Compiler.TypeSystem.Utils.Class
import Compiler.TypeSystem.Solver
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.Solver.Composable
import Compiler.TypeSystem.Utils.Infer
import Control.Monad.Extra (concatMapM)
import Compiler.TypeSystem.Kind.Infer.Type (infer'type)


infer'whole'program :: Program -> Infer'Env -> Infer'State -> Either Error (Type'Env, Kind'Env, Infer'State)
infer'whole'program program infer'env infer'state = do
  let base't'env = type'env infer'env
  ((t'env, k'cs), inf'state) <- run'infer infer'env (infer'types program) infer'state
  ((k'env, subst), inf'state') <- run'infer infer'env (infer'kinds program k'cs) inf'state

  -- TODO:  I will need to get the Kind Substitution from the kind inference and apply it to the Typing Context too.
  --        Because I need the Kinds in those type variables inside the types, to get assigned correct Kinds.
  --        I am not sure how it's going to work thought. I might need to write a new instance of Substitutable - for substituting Kind inside Scheme.


  return (apply subst t'env `Map.union` apply subst base't'env, k'env, inf'state')


-- TODO: I also need to check the types of methods
infer'types :: Program -> Infer (Type'Env, [Constraint Kind])
infer'types Program{ bind'sections = bgs, methods = methods, method'annotations = m'anns } = do
  -- ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
  (preds, assumptions, cs't, cs'k) <- infer'seq infer'bind'section bgs
  {-  TODO: Maybe it's not a best idea to put the `infer'method` itself right here.
            Maybe it's mixing the abstractions.
            I could write a helper functions for both lines - two functions which would just take `bgs` and `methods`
            and would produce a tuples. Maybe I shoudl do that. So that functions like `infer'program` read more like a description and contain less implementation details.
  -}
  (preds', cs't', cs'k') <- check'seq infer'method methods

  -- Question:  So all of the constraints were already solved in smaller groups of them.
  --            Do I expect some different result from solving them all?
  --            I think I can imagine getting an error now, even thought they were fine in smaller groups.
  --            But if I don't get an error, is the substitution any different then something like a merge of all the smaller substitutions?
  -- TODO:  Investigate. Do some experiments to be able to tell.

  -- Question:  So what exactly are the `preds` needed for?
  --            I can see that I apply the defaulting substitution to them.
  --            So do I need the retained predicates to figure out ambiguities?
  --            If there are no ambiguities, do I get an empty list in `preds`?
  -- TODO:  Investigate. I want to know if it's going to be empty, if I only write declarations which are not ambiguous.
  case run'solve (cs't ++ cs't') :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let rs = runIdentity $ runExceptT $ reduce c'env (apply subst (preds ++ preds'))
      case rs of
        Left err -> throwError err
        Right rs' -> do
          case runIdentity $ runExceptT $ default'subst c'env [] rs' of
            Left err -> throwError err
            Right s' -> do
              case runIdentity $ runExceptT (s' `merge` subst) of
                Left err -> throwError err
                Right subst' -> do
                  -- NOTE:  Just testing what happens if I apply the substitution to the method annotations
                  --        It shoulnd't do any harm. It also shouldn't really have any effect.
                  --        The type scheme given by the programmer should not change
                  -- let ms = map (\ (n, q't) -> (n, close'over q't)) m'anns
                  -- return (apply subst' $ Map.fromList $ assumptions ++ ms, cs'k ++ cs'k')

                  return (apply subst' $ Map.fromList assumptions, cs'k ++ cs'k')


infer'kinds :: Program -> [Constraint Kind] -> Infer (Kind'Env, Subst Name Kind)
infer'kinds Program{ data'declarations = data'decls, method'annotations = m'anns } k'cs = do
  k'data'constrs <- concatMapM collect data'decls
  k'method'constrs <- concatMapM infer'method m'anns

  -- TODO:  I think I will need to collect all type constructors declared by the user and associate them with their Kinds.
  --        That will give me a Kind Environment which I can later apply the Kind Substitution to.
  --        Maybe I do already have that in the Program?

  case run'solve (k'cs ++ k'data'constrs ++ k'method'constrs) :: Either Error (Subst Name Kind) of
    Left err -> throwError err
    Right subst -> do
      Infer'Env{ kind'env = k'env } <- ask
      Counter{ counter = counter } <- get
      return (apply subst k'env, subst)


    where
      collect :: Data -> Infer [Constraint Kind]
      collect Data{ type'name = t'n@(T'C t'name k), type'params = t'params, constructors = cons } = do
        -- potrebuju tohle - musim vyrobit tu typovou aplikaci t'name na vsechny type parametry a tomu priradit Kind *
        -- nejsem si tim uplne jistej, ale myslim, ze to by melo uplne stacit
        -- tim chci rict, ze nebude potreba prochazet jednotlivy typovy parametry - spojit jejich Kindy do Kind'Arrow
        -- a rict, ze ten typovej constructor t'name ma kind -- ta Kind'Arrow
        -- na druhou stranu bych to asi udelat mohl
        -- ale zajimalo by me, jestli by fakt nestacilo rict
        -- ze t'name aplikovanej na t'params == *
        -- mozna bych tim mohl zacit, zkusit udelat par testu a zjistit, jestli je mozny, aby se to v nejakem edge casu pokazilo, nebo jestli je ten algoritmus sound uz tak jak to je

        -- pak jeste musim projit vsechny constructory, bude potreba projit kazdej z nich
        -- a priradit kazdemu typu jednotlivych argumentu toho construktoru *

        -- let t'app = foldl T'App (T'Con t'name) $ map T'Var t'params
            -- t'k = kind t'app `Unify` K'Star
        -- ja nemuzu tohle udelat - protoze `kind` funkce predpoklada, ze Kind'App bude strukturovana tak, ze jeji Left bude Kind'Arrow - jenze v tenhle moment to spis budou
        -- Kind'Variable vetsinou - no a to zpusobi fail
        -- napada me reseni tak, ze `kind` bude vracet krome Kindu jeste list constraintu pro to, aby to tak bylo spravne
        -- coz by vlastne udelal presne to co jsem popisoval nahore
        -- ja bych rekl - kind cele tenhla aplikace je *
        -- a uz jenom tim, ze bych rekl, ze chci `kind` te aplikace, tak by mi to vyhazelo hromadu constraintu, ktery by assertovaly, ze ta vec je validne strukturovana 
        -- to mi prijde jako zajimavy napad - musim zkontrolovat kde vsude tuhle funkci `kind` pouzivam

        -- prozatim to obejdu a vyresim to tady na miste
        -- TODO: ale zvaz upraveni `kind` funkce - v tom pripade je fakt se potreba ujistit, ze to bude spravne fungovat vsude kde se to bude pozuivat
        -- let k'arr = foldr (\ (T'V _ k) k'acc -> K'Arr k k'acc ) K'Star t'params
        --     k'constr = k `Unify` k'arr
        --     constructors'k'cs = concatMap collect'con cons
        -- in  k'constr : constructors'k'cs

        -- to nahore taky neni spravne
        -- od ceho mam kind inferenci?
        let t = foldl (\ t'acc t'v -> T'App t'acc $ T'Var t'v) (T'Con t'n) t'params
        
        (k, k'cs) <- infer'type t

        let k'c = k `Unify` K'Star
        
        constructors'k'cs <- concatMapM collect'con cons

        return $ k'c : k'cs ++ constructors'k'cs

      infer'method :: (Name, Qualified Type) -> Infer [Constraint Kind]
      infer'method a@(n, context :=> type') = do
        -- NOTE:  I don't care about the Context
        -- all type variables there are just going to be in Predicates like (Show a), (Read a) and never like (Foo (m a))
        --
        infer'n'unify type'
        -- (k, k'cs) <- infer'type type'
        -- let cs =  (k `Unify` K'Star) : k'cs

        -- let tt = trace ("\n\n[++tracing++]   a:  " ++ show a ++ "\n  k:  " ++ show k ++ "\n  cs:  " ++ show cs) cs

        -- return tt



      infer'n'unify :: Type -> Infer [Constraint Kind]
      infer'n'unify t = do
        (k, k'cs) <- infer'type t
        return $ (k `Unify` K'Star) : k'cs

      collect'con :: Constr'Decl -> Infer [Constraint Kind]
      collect'con (Con'Decl _ types) = do
        -- kinds <- foldM infer'many [] types
        concatMapM infer'n'unify types

      collect'con (Con'Record'Decl _ fields) = do
        concatMapM infer'n'unify $ map snd fields

-- TODO: go over all the Data declarations
-- don't forget to create new constraints
-- infer Kinds from every Type annotation in the program - that means, going over the function bindings too?
--    maybe I don't need to? since I've already been there? But did I do collect the important constraints?
-- TODO: check the Frea's codebase
