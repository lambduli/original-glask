module Compiler.Syntax.Signature where


import Compiler.Syntax.Type (Type)
import Compiler.Syntax.Qualified
import Compiler.Syntax.Name


data Signature = T'Signature Name (Qualified Type)
  deriving (Eq)


instance Show Signature where
  show (T'Signature name qual'type) = name ++ " :: " ++ show qual'type
