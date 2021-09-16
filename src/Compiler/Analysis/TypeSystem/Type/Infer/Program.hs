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
import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Composable
import Compiler.Analysis.TypeSystem.Utils.Infer



infer'program :: Program -> Infer (Type'Env, [Constraint Kind])
infer'program bgs = do
  -- ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
  (preds, assumptions, cs't, cs'k) <- infer'seq infer'bind'section bgs

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
