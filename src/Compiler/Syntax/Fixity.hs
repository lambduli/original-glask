module Compiler.Syntax.Fixity where


data Fixity
  = Infixl
  | Infix
  | Infixr
  | Prefix
  | Postfix
  deriving (Eq)


instance Show Fixity where
  show Infixl = "infixl"
  show Infix = "infix"
  show Infixr = "infixr"
  show Prefix = "prefix"
  show Postfix = "postfix"
