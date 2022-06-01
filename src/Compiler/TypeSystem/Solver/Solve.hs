module Compiler.TypeSystem.Solver.Solve where


import Data.Functor.Identity

import Control.Monad.Except

import Compiler.TypeSystem.Error


-- | Constraint solver monad
-- type Solve a = ExceptT Error Identity a
