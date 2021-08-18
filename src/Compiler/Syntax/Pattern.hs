module Compiler.Syntax.Pattern where


import Compiler.Syntax.Literal (Literal)
import Compiler.Syntax.Name

data Pattern
  = P'Var Name
  | P'Con Name [Pattern]
  | P'Lit Literal
  | P'Wild
  deriving (Show, Eq)