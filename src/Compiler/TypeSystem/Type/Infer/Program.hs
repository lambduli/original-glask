{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Type.Infer.Program where

import Data.Maybe ( mapMaybe )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except ( runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad.State ( MonadState(get) )
import Control.Monad.Extra ( concatMapM )


import Compiler.Counter ( Counter(Counter, counter), State (get'counter) )

import Compiler.Syntax.Declaration ( Constr'Decl(..), Data(..) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'C(T'C), T'V' (T'V'), Type(..), M'V (Tau) )
import Compiler.Syntax.BindGroup ( Bind'Group(..) )
import qualified Compiler.Syntax.Overloaded as Overloaded


import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Method (Method) )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..), Kind'Env, Type'Env, Constraint'Env )
import Compiler.TypeSystem.InferenceState ( Infer'State(..) )
import qualified Compiler.TypeSystem.InferenceState as I'State

import Compiler.TypeSystem.Infer ( run'infer, Infer, Type'Check, get'constraints )
-- import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Type.Infer.BindSection ( check'seq, infer'bind'section, infer'seq )
import Compiler.TypeSystem.Type.Infer.Method ( infer'method )
import Compiler.TypeSystem.Kind.Infer.Type (infer'type)
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst (Sub), empty'subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term (free'vars) )
import Compiler.TypeSystem.Solver.Composable ( Composable(merge, compose) )
import Compiler.TypeSystem.Utils.Infer ( default'subst, overload, merge'into't'env )
import Compiler.TypeSystem.Utils.Class ( reduce )
import Compiler.TypeSystem.ClassEnv ( Class'Env )

import Compiler.TypeSystem.Kind.Infer.Program ( infer'kinds )
import Compiler.TypeSystem.Kind.Infer.TypeSection ( infer'annotated, infer'methods )
import Compiler.TypeSystem.Type.Infer.Declaration ( eliminate, eliminate'methods )


import Debug.Trace


infer'whole'program :: Program -> Infer'Env -> Counter -> Either Error (Program, Type'Env, Kind'Env, Constraint'Env, Counter, Class'Env)
infer'whole'program program infer'env counter = do
  let k'infer'state = Infer'State{ I'State.counter = counter, constraints = [] }

  ((k'env, class'env', kind'subst), k'infer'state') <- run'infer infer'env (infer'kinds program ) k'infer'state

  let base't'env  = type'env infer'env
      base'k'env  = kind'env infer'env
      base'cl'env = constraint'env infer'env

  let new'k'env         = k'env `Map.union` base'k'env
      new'class'env     = class'env' `Map.union` base'cl'env
      substituted't'env = apply kind'subst (type'env infer'env)
      
      cl'env              = class'env infer'env
      substituted'cl'env  = apply kind'subst cl'env

      ooo = trace ("{{ whole program }}\n  cl'env: " ++ show cl'env ++ "\n\n  ||  substed'cl'env: " ++ show substituted'cl'env ++ "\n\n ||  data and shit: " ++ show (data'n'class'sections program)) substituted'cl'env

  let Program{ bind'section = (explicits, implicits'list), methods = methods } = program
      infer'env' = infer'env{ kind'env = new'k'env
                            , type'env = substituted't'env
                            , constraint'env = new'class'env
                            , kind'substitution = kind'subst
                            , class'env = ooo {- substituted'cl'env -} }
                            -- NOTE: I need to put the kind substitution into the typing environment
                            --        when I later encounter any typing annotation (in the declaration or inline)
                            --        I first need to apply this substitution to it to fully specify the Kinds in it.
      oo = trace ("methods: " ++ show methods) explicits

      substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) oo -- explicits
      substituted'methods   = map (\ (Method sigma b'g cl'n d'n) -> Method (apply kind'subst sigma) b'g cl'n d'n) methods

  {-  NOTES:  It's OK for me to use the same `infer'env'` - the idea is - the isolated kind inference and substitution
              on Explicits and Methods should never influence/change anything in the inference environment.
              Specificaly - it must not change kind'context nor class'context - there's no way
              some lone type signature to some function or method would be able to do that.
  -}
  (new'expls, k'infer'state'') <- run'infer infer'env' (infer'annotated substituted'explicits) k'infer'state'

  (new'methods, k'infer'state''') <- run'infer infer'env' (infer'methods substituted'methods) k'infer'state''

  let program' = program{ bind'section = (new'expls, implicits'list), methods = new'methods }


  let counter' = get'counter k'infer'state'''
      t'infer'state = Infer'State{ I'State.counter = counter', constraints = [] }

  ((t'env, program''), t'infer'state') <- run'infer infer'env' (infer'types program') t'infer'state

  let new't'env = t'env `Map.union` substituted't'env -- it shouldn't matter which direction this is
      counter''  = get'counter t'infer'state'

  {-  NOTES:
      I don't need to apply the kind'susbt to the k'env.
      k'env is something that was created by the kind inference. In that process - only fully specified kind assumptions were ever returned.
      And since k'env is composed from those assumptions - it will always be fully substituted.
      For that reason I take left biased union between the new and the old environment.

      Same works for class'env.
  -}
  return (program'', new't'env, new'k'env, new'class'env, counter'', substituted'cl'env)


infer'types :: Program -> Type'Check (Type'Env, Program)
infer'types prg@Program{ bind'section = bg, methods = methods, method'annotations = m'anns } = do
  -- TODO: I first need to register all method constants as methods
  let overloads = map (\ (n, _, cl'name) -> (n, Overloaded.Method cl'name)) m'anns -- (Method _ Bind'Group{ name = n } cl'name)j


  -- ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
  (bg', preds, assumptions) <- overload overloads $ infer'bind'section bg

  let oo = trace ("\n\n{ with placeholders }  bg': " ++ show bg') bg'

  -- let message = "{{ tracing infer'types }} "
  --             ++ "\n|  preds: " ++ show preds
  --             ++ "\n|  assumptions: " ++ show assumptions
  --     a = trace message preds

  {-  TODO: Maybe it's not a best idea to put the `infer'method` itself right here.
            Maybe it's mixing the abstractions.
            I could write a helper functions for both lines - two functions which would just take `bgs` and `methods`
            and would produce a tuples. Maybe I shoudl do that. So that functions like `infer'program` read more like a description and contain less implementation details.
  -}
  -- NOTE: problem is - now methods are analyzed and translated without the information about explicits and implicits
  -- the assumptions contain all of thath information, so I will need to register them again
  let overloads' = mapMaybe (\ (n, T'Forall _ (ctxt :=> _)) -> if null ctxt then Nothing else Just (n, Overloaded.Overloaded)) assumptions
  (methods', preds') <- overload (overloads ++ overloads') $ merge'into't'env assumptions $ check'seq infer'method methods

  let ee = trace ("\n\n { overloads methods }  overloads: " ++ show overloads ++ "\n   | methods: " ++ show methods ++ "\n   |  methods with placeholders?: " ++ show methods') methods'

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
  
  cs't <- get'constraints
  case run'solve cs't {- (cs't ++ cs't') -} :: Either Error (Subst M'V Type) of
    Left err -> trace "here 1" throwError err
    Right subst -> do
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      -- return (apply subst $ Map.fromList assumptions)


      let rs = runIdentity $ runExceptT $ reduce c'env (apply subst (preds ++ preds'))
      case rs of
        Left err -> do
          throwError err
        Right rs' -> do
          case runIdentity $ runExceptT $ default'subst c'env [] rs' of
            Left err -> do
              -- let message = "[[[ tracing infer'types ]]]"
              --             ++ "\n|  c'env: " ++ show c'env
              --             ++ "\n|  result: " ++ show (preds, assumptions, cs't)
              --             ++ "\n|  rs': " ++ show rs'
              --     t = trace message err
              throwError err
            Right s' -> do
              case runIdentity $ runExceptT (s' `merge` subst) of
                Left err -> throwError err
                Right subst' -> do
                  let sub = subst'
                  let properly'close :: (Name, Sigma'Type) -> ((Name, Sigma'Type), Subst M'V Type)
                      properly'close (name, sigma@(T'Forall tvs qual'type)) =
                        let mapping = map (\ (T'V' name kind) -> (Tau name kind, T'Var' (T'V' name kind)) ) tvs
                            sub = Sub $ Map.fromList mapping
                            sigma' = apply sub sigma
                        in ((name, sigma'), sub)
                      properly'close _ = error "unexpected: assumption from local declaration is not forall"
                      {-  IMPORTANT NOTE: The idea behind properly close is interesting and important one.
                                          I have a sigma type - meaning it has already been decided which variables it should quantify over.
                                          I can't change that here.
                                          But what is also happening is after the substitution there is going to be some meta variables which need to become normal bound variables.
                                          Because - as I said above - it has already been decided (inside Implicit I think).
                                          So I carefully create substitution which only changes those quantified variables from meta to bound. That's all.  -}

                      assum'applied = apply sub assumptions
                      (assum'closed, substs) = unzip $ map properly'close assum'applied
                      rr = trace (">>> substs: " ++ show substs ++ "   |||  sub: " ++ show sub) substs
                      composed'substs = foldl compose sub rr -- substs -- I think this should work, compose does not require all of them to agree
                      -- more importantly, it has to compose with the original substitution, I was missing that, and it didn't work
                  let xx = trace ("\n\n OOOOOO || methods to eliminate: " ++ show methods' ++ "\n  assum'closed " ++ show assum'closed ++ "\n  composed'substs " ++ show composed'substs ) ee
                      
                  -- TODO: I need to call eliminate in the bind group too, give it the correct substitution and assumptions
                  bg'' <- eliminate composed'substs assum'closed oo -- bg'

                  let zz = trace ("\n\n ELIMINATED BINDINGS: " ++ show bg'' ++ "\n COMPOSED'SUBSTS: " ++ show composed'substs) bg''

                  -- AND METHODS NEED THAT TOO!!!
                  methods'' <- eliminate'methods composed'substs assum'closed xx -- methods'


                      -- message = "<<< tracing >>>   "
                      --   ++ "\n|  sub " ++ show sub
                      --   ++ "\n|  assumptions: " ++ show assumptions
                      --   ++ "\n|  apply subst assumptions: " ++ show assum'applied
                      --   ++ "\n|  properly close assumptions: " ++ show assum'closed
                      --   ++ "\n|  result: " ++ show (apply sub $ Map.fromList assumptions)
                      --   ++ "\n|"
                      -- ss = trace message assum'closed
                  {-  QUESTION: Shouldn't I somehow check that the defaulting substitution effectivelly eliminates all the predicates?  -}
                  {-            Or is it OK if there are some predicates which bubble-up to FROM the top level declarations?            -}
                  {-            If they are not eliminated by the defaulting substitution, they are effectively unsolved right?  -}
                  {-  ANSWER:   That's what `default'subst` does - it fails if it can not get rid of them.  -}

                  -- NOTE:  Just testing what happens if I apply the substitution to the method annotations
                  --        It shoulnd't do any harm. It also shouldn't really have any effect.
                  --        The type scheme given by the programmer should not change
                  -- let ms = map (\ (n, q't) -> (n, close'over q't)) m'anns
                  -- return (apply subst' $ Map.fromList $ assumptions ++ ms, cs'k ++ cs'k')

                  return (Map.fromList assum'closed, prg{ bind'section = zz, methods = methods'' })
