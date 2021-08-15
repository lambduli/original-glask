module Compiler.Syntax.Signature where

import Compiler.Syntax.Type (Type)


data Signature = T'Signature String Type


instance Show Signature where
  show (T'Signature name type') = name ++ " :: " ++ show type'
