module Compiler.Syntax.Fixity where


data Fixity
  = Infixl
  | Infix
  | Infixr
  deriving (Eq)


instance Show Fixity where
  show Infixl = "infixl"
  show Infix = "infix"
  show Infixr = "infixr"
