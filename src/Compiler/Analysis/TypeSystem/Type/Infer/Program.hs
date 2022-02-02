module Compiler.Analysis.TypeSystem.Type.Infer.Program where

import qualified Data.Map.Strict as Map
import Control.Monad.Except
import Control.Monad.Reader
import Data.Functor.Identity

import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Program
import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint

import Compiler.Analysis.TypeSystem.Type.Infer.BindSection

import Compiler.Analysis.TypeSystem.Utils.Class

import Compiler.Analysis.TypeSystem.Solver
import Compiler.Analysis.TypeSystem.Solver.Substitution
import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Composable
import Compiler.Analysis.TypeSystem.Utils.Infer


-- TODO: I also need to check the types of methods
infer'program :: Program -> Infer (Type'Env, [Constraint Kind])
infer'program Program{ bind'sections = bgs, methods = methods } = do
  -- ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
  (preds, assumptions, cs't, cs'k) <- infer'seq infer'bind'section bgs

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
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let rs = runIdentity $ runExceptT $ reduce c'env (apply subst preds)
      case rs of
        Left err -> throwError err
        Right rs' -> do
          case runIdentity $ runExceptT $ default'subst c'env [] rs' of
            Left err -> throwError err
            Right s' -> do
              case runIdentity $ runExceptT (s' `merge` subst) of
                Left err -> throwError err
                Right subst' -> do
                  return (apply subst' $ Map.fromList assumptions, cs'k)
