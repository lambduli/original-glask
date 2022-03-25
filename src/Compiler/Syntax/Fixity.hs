module Compiler.Syntax.Fixity where


-- data Fixity
--   = Infixl
--   | Infix
--   | Infixr
--   | Prefix
--   | Postfix
--   deriving (Eq)

data Fixity = Prefix | Infix | Postfix
  deriving (Eq)


instance Show Fixity where
  show Infix = "infix"
  show Prefix = "prefix"
  show Postfix = "postfix"
