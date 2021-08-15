module Compiler.Syntax.Pattern where


import Compiler.Syntax.Literal (Literal)

data Pattern
  = P'Var String
  | P'Con String [Pattern]
  | P'Lit Literal
  | P'Wild
  deriving (Show, Eq)