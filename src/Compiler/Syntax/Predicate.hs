module Compiler.Syntax.Predicate where


import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type )
import Compiler.Syntax.HasKind ( HasKind(kind) )


data Predicate = Is'In Name Type
  deriving (Eq)


instance Show Predicate where
  show (Is'In class'name type')
    = class'name ++ " " ++ show type'
