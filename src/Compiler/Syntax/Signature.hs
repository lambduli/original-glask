module Compiler.Syntax.Signature where


import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type )
import Compiler.Syntax.Name ( Name )


data Signature = T'Signature Name Sigma'Type
  deriving (Eq)


{-  INVARIANT:  all type signatures are well-formed. That means - closed under the explicit forall. -}
instance Show Signature where
  show (T'Signature name sigma'type) = name ++ " :: " ++ show sigma'type
