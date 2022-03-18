module Compiler.Analysis.Syntactic.ConstraintEnv where

import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Kind ( Kind )


type Constraint'Env = Map.Map Name Kind
