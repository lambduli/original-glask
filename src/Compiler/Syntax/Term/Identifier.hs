module Compiler.Syntax.Term.Identifier where


import Compiler.Syntax.Name


data Term'Id
  = Term'Id'Var Name
  | Term'Id'Const Name
  deriving (Eq, Ord, Show)
  -- NOTE: I think it's safe to derive Ord, because it's only going to be used in the Set
