module Compiler.Analysis.Syntactic.FieldEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import Compiler.Syntax.Type

import Compiler.Analysis.Syntactic.ConstrEnv

type Field'Env = Map.Map Name Constr'Info
