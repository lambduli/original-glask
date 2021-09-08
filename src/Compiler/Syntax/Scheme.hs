module Compiler.Syntax.Scheme where

import Compiler.Syntax.Type
import Compiler.Syntax.Qualified


data Scheme
  = For'All [T'V] (Qualified Type) -- TODO: continue here - refactor according the paper
  deriving (Eq)


instance Show Scheme where
  show (For'All [] qual'type')
    = show qual'type'
  show (For'All type'args qual'type')
    = "forall " ++ unwords (map show type'args) ++ " . " ++ show qual'type'
