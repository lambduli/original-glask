module Compiler.Syntax.Qualified where


import Data.List ( intercalate )


import Compiler.Syntax.Predicate ( Predicate )


data Qualified t = [Predicate] :=> t
  deriving (Eq)


instance Show t => Show (Qualified t) where
  show ([] :=> t)
    = show t
  show ([pred] :=> t)
    = show pred ++ " => " ++ show t
  show (preds :=> t)
    = "(" ++ intercalate ", " (map show preds ) ++ ") => " ++ show t
