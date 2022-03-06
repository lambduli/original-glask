module Compiler.TypeSystem.Type.Infer.Program where

import qualified Data.Map.Strict as Map
import Control.Monad.Except ( runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad.State ( MonadState(get) )
import Control.Monad.Extra ( concatMapM )


import Compiler.Counter ( Counter(Counter, counter) )

import Compiler.Syntax.Declaration ( Constr'Decl(..), Data(..) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'C(T'C), T'V, Type(..) )


import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Method (Method) )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..), Kind'Env, Type'Env, Constraint'Env )
import Compiler.TypeSystem.InferenceState ( Infer'State )
import Compiler.TypeSystem.Infer ( run'infer, Infer )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Type.Infer.BindSection ( check'seq, infer'bind'section, infer'seq )
import Compiler.TypeSystem.Type.Infer.Method ( infer'method )
import Compiler.TypeSystem.Kind.Infer.Type (infer'type)
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )
import Compiler.TypeSystem.Solver.Composable ( Composable(merge) )
import Compiler.TypeSystem.Utils.Infer ( default'subst )
import Compiler.TypeSystem.Utils.Class ( reduce )

import Compiler.TypeSystem.Kind.Infer.Program ( infer'kinds )
import Compiler.TypeSystem.Kind.Infer.TypeSection ( infer'annotated, infer'methods )


infer'whole'program :: Program -> Infer'Env -> Infer'State -> Either Error (Type'Env, Kind'Env, Constraint'Env, Infer'State)
infer'whole'program program infer'env infer'state = do
  ((k'env, class'env, kind'subst), infer'state') <- run'infer infer'env (infer'kinds program) infer'state
  
  let base't'env  = type'env infer'env
      base'k'env  = kind'env infer'env
      base'cl'env = constraint'env infer'env

  let new'k'env         = k'env `Map.union` base'k'env
      new'class'env     = class'env `Map.union` base'cl'env
      substituted't'env = apply kind'subst (type'env infer'env)

  let Program{ bind'section = (explicits, implicits'list), methods = methods } = program
      infer'env' = infer'env{ kind'env = new'k'env
                            , type'env = substituted't'env
                            , constraint'env = new'class'env
                            , kind'substitution = kind'subst }
                            -- NOTE: I need to put the kind substitution into the typing environment
                            --        when I later encounter any typing annotation (in the declaration or inline)
                            --        I first need to apply this substitution to it to fully specify the Kinds in it.

      substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) explicits
      substituted'methods   = map (\ (Method sigma b'g) -> Method (apply kind'subst sigma) b'g) methods

  {-  NOTES:  It's OK for me to use the same `infer'env'` - the idea is - the isolated kind inference and substitution
              on Explicits and Methods should never influence/change anything in the inference environment.
              Specificaly - it must not change kind'context nor class'context - there's no way
              some lone type signature to some function or method would be able to do that.
  -}
  (new'expls, infer'state'') <- run'infer infer'env' (infer'annotated substituted'explicits) infer'state'

  (new'methods, infer'state''') <- run'infer infer'env' (infer'methods substituted'methods) infer'state''
      
  let program' = program{ bind'section = (new'expls, implicits'list), methods = new'methods }


  (t'env, inf'state) <- run'infer infer'env' (infer'types program') infer'state'''

  let new't'env = t'env `Map.union` substituted't'env -- it shouldn't matter which direction this is

  {-  NOTES:
      I don't need to apply the kind'susbt to the k'env.
      k'env is something that was created by the kind inference. In that process - only fully specified kind assumptions were ever returned.
      And since k'env is composed from those assumptions - it will always be fully substituted.
      For that reason I take left biased union between the new and the old environment.

      Same works for class'env.
  -}
  return (new't'env, new'k'env, new'class'env, inf'state)


infer'types :: Program -> Infer Type'Env
infer'types Program{ bind'section = bg, methods = methods } = do
  -- ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
  (preds, assumptions, cs't) <- infer'bind'section bg

  {-  TODO: Maybe it's not a best idea to put the `infer'method` itself right here.
            Maybe it's mixing the abstractions.
            I could write a helper functions for both lines - two functions which would just take `bgs` and `methods`
            and would produce a tuples. Maybe I shoudl do that. So that functions like `infer'program` read more like a description and contain less implementation details.
  -}
  (preds', cs't') <- check'seq infer'method methods

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
                  {-  QUESTION: Shouldn't I somehow check that the defaulting substitution effectivelly eliminates all the predicates?  -}
                  {-            Or is it OK if there are some predicates which bubble-up to FROM the top level declarations?            -}
                  {-  What I meant was - if they are not eliminated by the defaulting substitution, they are effectively unsolved right?
                      But maybe the function default'subst fails if it can't eliminate all of them. I think that's the case and the reason
                      why it's OK. -}

                  -- NOTE:  Just testing what happens if I apply the substitution to the method annotations
                  --        It shoulnd't do any harm. It also shouldn't really have any effect.
                  --        The type scheme given by the programmer should not change
                  -- let ms = map (\ (n, q't) -> (n, close'over q't)) m'anns
                  -- return (apply subst' $ Map.fromList $ assumptions ++ ms, cs'k ++ cs'k')

                  return (apply subst' $ Map.fromList assumptions)
