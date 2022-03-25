module Compiler.Analysis.Syntactic.FixityEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Fixity ( Fixity )
import Compiler.Syntax.Associativity ( Associativity )


type Fixity'Env = Map.Map Name Fixity'Info


type Fixity'Info = (Fixity, Associativity, Int)
