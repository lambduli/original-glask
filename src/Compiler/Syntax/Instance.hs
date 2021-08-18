module Compiler.Syntax.Instance where


import Compiler.Syntax.Qualified
import Compiler.Syntax.Predicate


{- TODO: NOTE: I will most likely need to also store definitions of the methods and constants -}
type Instance = Qualified Predicate