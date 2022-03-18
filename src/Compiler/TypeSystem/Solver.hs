{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.TypeSystem.Solver where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad.Except ( runExceptT )

import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Constraint ( Constraint(..) )
import Compiler.TypeSystem.Solver.Solve ( Solve )
import Compiler.TypeSystem.Solver.Substitution ( empty'subst, Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(..) )
import Compiler.TypeSystem.Solver.Composable ( Composable(compose) )
import Compiler.TypeSystem.Solver.Unify ( Unify(..) )


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
    ((left `Match` right) : constrs) -> do
      subst' <- left `match` right
      solver (subst' `compose` subst, apply subst' constrs)
    ((left `Unify` right) : constrs) -> do
      subst'  <- left `unify` right
      solver (subst' `compose` subst, apply subst' constrs)
