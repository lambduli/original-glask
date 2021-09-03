module Compiler.Analysis.TypeSystem.Solver.Solve where


import Data.Functor.Identity

import Control.Monad.Except

import Compiler.Analysis.Error


-- | Constraint solver monad
type Solve a = ExceptT Error Identity a
