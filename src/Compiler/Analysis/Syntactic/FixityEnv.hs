module Compiler.Analysis.Syntactic.FixityEnv where

import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import Compiler.Syntax.Fixity


type Fixity'Env = Map.Map Name Fixity'Info


type Fixity'Info = (Fixity, Int)
