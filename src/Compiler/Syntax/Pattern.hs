module Compiler.Syntax.Pattern where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal (Literal)

import Compiler.Syntax.Type ( Sigma'Type )


data Pattern
  = P'Var Name
  | P'Con Name [Pattern]
  | P'Lit Literal
  | P'As Name Pattern
  | P'Wild
  | P'Ann Pattern Sigma'Type
  deriving (Show, Eq)