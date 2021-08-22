module Compiler.Syntax.Term.Identifier where


import Compiler.Syntax.Name


data Term'Id
  = Term'Id'Var Name
  | Term'Id'Const Name
  deriving (Eq)
