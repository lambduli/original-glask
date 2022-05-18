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
  deriving (Eq)


instance Show Pattern where
  show (P'Var name) = name
  show (P'Con tag patterns) = "(" ++ tag ++ " " ++ unwords (map show patterns) ++ ")"
  show (P'Lit lit) = show lit
  show (P'As alias pattern) = alias ++ "@" ++ show pattern
  show P'Wild = "_"
  show (P'Ann pattern sigma) = "(" ++ show pattern ++ " :: " ++ show sigma ++ ")"