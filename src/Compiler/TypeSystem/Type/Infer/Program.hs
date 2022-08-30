{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Type.Infer.Program where

import Data.Maybe ( mapMaybe )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except ( runExceptT, MonadError(throwError), unless )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad.State ( MonadState(get) )
import Data.Bifunctor (Bifunctor(bimap, first, second))


import Compiler.Counter ( Counter(Counter), State (get'counter) )

import Compiler.Syntax.Declaration ( Constr'Decl(..), Data(..) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'C(T'C), T'V' (T'V'), Type(..), M'V (Tau) )
import Compiler.Syntax.BindGroup ( Bind'Group(..) )
import qualified Compiler.Syntax.Overloaded as Overloaded


import Compiler.TypeSystem.Error ( Error (Typed'Holes) )
import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Method (Method) )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env, Kind'Env, Type'Env, Constraint'Env, Instances )
import qualified Compiler.TypeSystem.InferenceEnv as I'Env
import Compiler.TypeSystem.InferenceState ( Infer'State (holes) )
import qualified Compiler.TypeSystem.InferenceState as I'State

import Compiler.TypeSystem.Infer ( run'infer, Infer, Type'Check, get'constraints, add'overloads )

import Compiler.TypeSystem.Type.Infer.BindSection ( check'seq, infer'bind'section )
import Compiler.TypeSystem.Type.Infer.Method ( infer'method )

import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst (Sub) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term )
import Compiler.TypeSystem.Solver.Composable ( Composable(merge, compose) )
import Compiler.TypeSystem.Utils.Infer ( default'subst, overload, merge'into't'env )
import Compiler.TypeSystem.Utils.Class ( reduce )
import Compiler.TypeSystem.ClassEnv ( Class'Env )

import Compiler.TypeSystem.Kind.Infer.Program ( infer'kinds )
import Compiler.TypeSystem.Kind.Infer.TypeSection ( infer'annotated, infer'methods )
import Compiler.TypeSystem.Type.Infer.Declaration ( eliminate, eliminate'methods )


infer'whole'program :: Program -> Infer'Env -> Counter -> Either Error (Program, Type'Env, Kind'Env, Constraint'Env, Counter, Class'Env, Infer'State Type, [((Name, Type), Name)])
infer'whole'program program infer'env counter = do
  let k'infer'state = I'State.Infer'State{ I'State.counter = counter, I'State.constraints = [], I'State.overloaded = [], I'State.instances = [], I'State.holes = [] }

  ((k'env, class'env', kind'subst), k'infer'state') <- run'infer infer'env (infer'kinds program ) k'infer'state

  let base't'env  = I'Env.type'env infer'env
      base'k'env  = I'Env.kind'env infer'env
      base'cl'env = I'Env.constraint'env infer'env

  let new'k'env         = k'env `Map.union` base'k'env
      new'class'env     = class'env' `Map.union` base'cl'env
      substituted't'env = apply kind'subst (I'Env.type'env infer'env)

      cl'env              = I'Env.class'env infer'env
      substituted'cl'env  = apply kind'subst cl'env

      inst'env              = I'Env.instance'env infer'env
      substituted'inst'env  = map (first $ second $ apply kind'subst) inst'env

      substituted'instances = map (bimap (second (apply kind'subst)) (\ (a, b, c) -> (a, apply kind'subst b, apply kind'subst c))) (I'Env.instances infer'env)
      -- this line is ugly but it just does a simple things - instances is a pair with a two-ple and tri-ple
      -- both elements need the substitution to happen on them, refactoring this code should be trivial

  let Program{ bind'section = (explicits, implicits'list), methods = methods } = program
      infer'env' = infer'env{ I'Env.kind'env = new'k'env
                            , I'Env.type'env = substituted't'env
                            , I'Env.constraint'env = new'class'env
                            , I'Env.kind'substitution = kind'subst
                            , I'Env.class'env = substituted'cl'env
                            , I'Env.instances = substituted'instances
                            , I'Env.instance'env = substituted'inst'env }
                            -- NOTE: I need to put the kind substitution into the typing environment
                            --        when I later encounter any typing annotation (in the declaration or inline)
                            --        I first need to apply this substitution to it to fully specify the Kinds in it.

      substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) explicits
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
  -- NOTE: the reason why I am taking the stuff from the inference environment is because I need to put it in the state, but the state only comes to existence here, and so I need to do that
      t'infer'state = I'State.Infer'State{ I'State.counter = counter', I'State.constraints = [], I'State.instances = substituted'instances, I'State.overloaded = I'Env.overloaded infer'env', I'State.holes = [] }

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
  return (program'', new't'env, new'k'env, new'class'env, counter'', substituted'cl'env, t'infer'state', substituted'inst'env)


infer'types :: Program -> Type'Check (Type'Env, Program)
infer'types prg@Program{ bind'section = bg, methods = methods, method'annotations = m'anns } = do
  -- TODO: I first need to register all method constants as methods
  let overloads = map (\ (n, _, cl'name) -> (n, Overloaded.Method cl'name)) m'anns -- (Method _ Bind'Group{ name = n } cl'name)j
  add'overloads overloads


  -- ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
  (bg', preds, assumptions) <- overload overloads $ infer'bind'section bg

  {-  TODO: Maybe it's not a best idea to put the `infer'method` itself right here.
            Maybe it's mixing the abstractions.
            I could write a helper functions for both lines - two functions which would just take `bgs` and `methods`
            and would produce a tuples. Maybe I shoudl do that. So that functions like `infer'program` read more like a description and contain less implementation details.
  -}
  -- NOTE: problem is - now methods are analyzed and translated without the information about explicits and implicits
  -- the assumptions contain all of thath information, so I will need to register them again
  let overloads' = mapMaybe (\ (n, T'Forall _ (ctxt :=> _)) -> if null ctxt then Nothing else Just (n, Overloaded.Overloaded)) assumptions
  (methods', preds') <- overload (overloads ++ overloads') $ merge'into't'env assumptions $ check'seq infer'method methods

  -- NOTE: I need to store method overloads into the Infer'State, for the REPL to operate on it too
  add'overloads overloads'

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
  case run'solve cs't :: Either Error (Subst M'V Type) of
    Left err -> throwError err
    Right subst -> do
      I'Env.Infer'Env{ I'Env.type'env = t'env, I'Env.class'env = c'env } <- ask

      let rs = runIdentity $ runExceptT $ reduce c'env (apply subst (preds ++ preds'))
      case rs of
        Left err -> do
          throwError err

        Right rs' -> do
          case runIdentity $ runExceptT $ default'subst c'env [] rs' of
            Left err -> do
              throwError err

            Right s' -> do
              case s' `compose` subst of  -- runIdentity $ runExceptT (s' `merge` subst) of
                -- Left err -> do
                --   throwError err
                --
                -- This was really embarassing moment. I think I wanted to get more safety and instead I introduced a massive bug into the system.
                -- I have a feeling, that the properly'closing (maybe some part of it) might have been influenced (caused) by this mistake. I am not sure though.

                -- Right subst' -> do
                subst' -> do
                  let sub = subst'
                  let properly'close :: (Name, Sigma'Type) -> ((Name, Sigma'Type), Subst M'V Type)
                      properly'close (name, sigma@(T'Forall tvs qual'type)) =
                        let mapping = map (\ (T'V' name kind) -> (Tau name kind, T'Var' (T'V' name kind)) ) tvs
                            sub = Sub $ Map.fromList mapping
                            -- sub = Sub Map.empty :: Subst M'V Type
                            sigma' = apply sub sigma
                            
                        in if (sigma /= sigma') then (error $ show (sigma') ++ "  |  " ++ show sigma ++ " =/= " ++ show sigma') else ((name, sigma'), sub)
                      properly'close _ = error "unexpected: assumption from local declaration is not forall"
                      {-  IMPORTANT NOTE: The idea behind properly close is interesting and important one.
                                          I have a sigma type - meaning it has already been decided which variables it should quantify over.
                                          I can't change that here.
                                          But what is also happening is after the substitution there is going to be some meta variables which need to become normal bound variables.
                                          Because - as I said above - it has already been decided (inside Implicit I think).
                                          So I carefully create substitution which only changes those quantified variables from meta to bound. That's all.  -}

                      assum'applied = apply sub assumptions
                      (assum'closed, substs) = unzip $ map properly'close assum'applied
                      composed'substs = foldl (flip compose) sub  substs -- I think this should work, compose does not require all of them to agree
                      closing'sub = foldl compose (Sub Map.empty) substs
                      -- more importantly, it has to compose with the original substitution, I was missing that, and it didn't work
                      -- NOTE: Turns out it does not really work that well. The issue is that compose uses left biased union
                      -- this means that when composing two substitutions that do not agree, we get inconsistency
                      -- IMPORTANT: I think it would be better to first apply the normal substitution and
                      -- then the composution of the closing ones (that composition should be safe)

                  -- TODO: handle typed holes
                  -- first apply the substitution on all typed holes
                  -- but also the properly'closing substitution - that one
                  state <- get
                  let holes' = apply closing'sub $ apply sub (holes state)
                  -- error $ show holes'
                  unless (null holes') (throwError $ Typed'Holes holes' assum'closed)
                  -- TODO: I should actually only fail when the type within the typed hole is fully specified - no meta variables
                  -- then I can report the most precise info

                    -- NOTE: If it does not fail, I don't need to put the holes' back in - because there are none
                  -- TODO: I need to call eliminate in the bind group too, give it the correct substitution and assumptions
                  bg'' <- eliminate composed'substs assum'closed bg'

                  -- AND METHODS NEED THAT TOO!!!
                  methods'' <- eliminate'methods composed'substs assum'closed methods'

                  {-  QUESTION: Shouldn't I somehow check that the defaulting substitution effectivelly eliminates all the predicates?  -}
                  {-            Or is it OK if there are some predicates which bubble-up to FROM the top level declarations?            -}
                  {-            If they are not eliminated by the defaulting substitution, they are effectively unsolved right?  -}
                  {-  ANSWER:   That's what `default'subst` does - it fails if it can not get rid of them.  -}

                  return (Map.fromList assum'closed, prg{ bind'section = bg'', methods = methods'' })
