module Compiler.TypeSystem.Type.Infer.Explicit where


import Control.Monad.Except ( filterM, runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Functor.Identity ( Identity(runIdentity) )
import qualified Data.Set as Set
import Data.List ( (\\) )


import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name, alternatives) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'V', Type (T'Forall), M'V )

import Compiler.TypeSystem.Error ( Error(..) )
import Compiler.TypeSystem.Infer ( Infer, Type'Check, get'constraints )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Binding ( Explicit(..) )
import Compiler.TypeSystem.Utils.Infer ( instantiate, split', skolemise, close'over' )
import Compiler.TypeSystem.Utils.Class ( entail )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, class'env) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Infer.Match ( check'matches )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'type )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'infer'sigma, kind'specify )
import Compiler.TypeSystem.Expected ( Expected(Check) )
import Compiler.TypeSystem.ClassEnv (Class'Env(Class'Env))


{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
{- Returning a [Constraint Type] might not be strictly necessary -}
infer'expl :: Explicit -> Type'Check (Explicit, [Predicate])
infer'expl (Explicit sigma bg@Bind'Group{ name = name, alternatives = matches }) = do
  -- do the kind analysis for the sigma
  -- in the process we also substitute kind meta variables with actual kinds of known types (and type classes)
  sigma' <- kind'specify sigma

  (skolems, qs, t) <- skolemise sigma'
  {-  Duvod proc pouzivam skolemise misto instantiate je jednoduchy. Jelikoz jde o typovou anotaci, musim skolemizovat.
      Otazka ale zustava - je skolemizace to jediny, co je treba udelat v tomhle miste jinak? Nebo bych mel zkontrolovat
      vic veci? Najdi si implementaci `infer'expr` pro Ann a porovnej s tim co se deje tam, neco podobnyho by se asi melo dit i tady si myslim.
  -}

  (matches', preds) <- check'matches matches t

  subst <- get'subst

  let qs' = apply subst qs
      t'  = apply subst t
      -- so I need to apply the substitution to the typing context
      -- because that is how it's done in the THIH paper
  Infer'Env{ type'env = t'env, class'env = c'env } <- ask
  let fs = Set.toList $ free'vars $ apply subst t'env
  {-  NOTE: fs are free type variables within the whole typing context, those might be variables which are types of restricted declarations and will be defaulted
            before they are defaulted though, they can easily be used by other declarations - like this one explicitly typed
            that is the reason why this one must not quantify over it, - simply put - it is not yours to quantify over (and it might even cease to be a type variable later - defaulting)
            one thing I am thinking about - could it happen that some implicit restricted thing would be inferred as Num (for example) from its own definition
            but then someone else uses it in such a way that it implies other type class(es) so then it might not be able to be defaulted?

            I tried it and it fails with unsatisfiable type class predicate/type context.
            Though in my example there was no defaulting, but I think it would just fail in the defaulting.

            Question though: Why do I care about free variables from type context and free type variables (both meta) introduced by other scopes?
            Why don't I just only care about the variables which I HAVE INTRODUCED?
            Those are the ones I CAN QUALIFY if I want. Otherwise I should not touch anything, right?
            Or is there some situation, where I could legally qualify over some variable which I have NOT introduced at this level?

            Or is it because it is not easy to know WHICH meta vars have I INTRODUCED?
            That might be it.
            Because I always get those by removing all "other meta vars" from those of mine.
    -}
      gs = Set.toList (free'vars t') \\ fs
  let sc' = close'over' (qs' :=> t')
      ps' = filter (not . entail c'env qs') (apply subst preds)
  
  (deferred'preds, retained'preds) <- split'' c'env fs skolems gs ps'

  if not (null retained'preds)
  then do
    throwError Context'Too'Weak
  else do
    -- because of the elaboration/elimination, we need to return a sigma type made from skolemised predicates and type
    -- as well as elaborated list of matches -> matches'
    return (Explicit (T'Forall skolems (qs :=> t)) bg{ name = name, alternatives = matches' }, deferred'preds)


get'subst :: Type'Check (Subst M'V Type)
get'subst = do
  cs't <- get'constraints

  case run'solve cs't :: Either Error (Subst M'V Type) of
    Left err -> do
      throwError err

    Right subst -> do
      return subst


-- lift :: Either Error a -> Type'Check a
-- lift preds = do
--   case preds of
--     Left err -> do
--       throwError err
    
--     Right preds -> do
--       return preds

split'' :: Class'Env -> [M'V] -> [T'V'] -> [M'V] -> [Predicate] -> Type'Check ([Predicate], [Predicate])
split'' c'env fs skolems gs preds'
  = case runIdentity $ runExceptT $ split' c'env fs skolems gs preds' of
    Left err -> throwError err
    Right pair -> return pair
    