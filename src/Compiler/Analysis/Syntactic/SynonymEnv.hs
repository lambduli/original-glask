module Compiler.Analysis.Syntactic.SynonymEnv where

import qualified Data.Map.Strict as Map

import Compiler.Syntax (Name)


{-  This structure associates a name of the type synonym with it's arity -}
type Synonym'Env = Map.Map Name Int
