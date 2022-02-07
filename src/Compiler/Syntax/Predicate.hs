module Compiler.Syntax.Predicate where


import Compiler.Syntax.Name
import Compiler.Syntax.Type


data Predicate = Is'In Name Type
  deriving (Eq)


instance Show Predicate where
  show (Is'In class'name type')
    = class'name ++ " " ++ show type'
