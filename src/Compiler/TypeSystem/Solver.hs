{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.TypeSystem.Solver where


import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity

import Control.Monad.Except


import Compiler.Syntax

import Compiler.TypeSystem.Error

import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.InferenceEnv

import Compiler.TypeSystem.Solver.Solve
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.Solver.Occurs
import Compiler.TypeSystem.Solver.Composable
import Compiler.TypeSystem.Solver.Bind
import Compiler.TypeSystem.Solver.Unify


run'solve :: (Ord k, Substitutable k a a, Unify a k a, Composable k a) => [Constraint a] -> Either Error (Subst k a)
run'solve constrs = runIdentity $ runExceptT $ solver state
  where state = (empty'subst, constrs)


{-  Unifier class represents ...
    `k` represents the type which indexes the Substitution
        usually String (in case of Kind unification) or T'V (in case of Type unification)
    `a` represents the value which is associated with `k` in the Substitution
        usually Kind or Type
-}
type Unifier k a = (Subst k a, [Constraint a])


-- Unification solver
solver :: (Ord k, Substitutable k a a, Unify a k a, Composable k a) => Unifier k a -> Solve (Subst k a)
solver (subst, constraints) =
  case constraints of
    [] -> return subst
    ((type'l `Unify` type'r) : constrs) -> do
      subst'  <- type'l `unify` type'r
      solver (subst' `compose` subst, apply subst' constrs)
