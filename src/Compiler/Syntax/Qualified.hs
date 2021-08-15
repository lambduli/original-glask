module Compiler.Syntax.Qualified where


import Data.List


import Compiler.Syntax.Name
import Compiler.Syntax.Type
import Compiler.Syntax.Predicate


data Qualified t = [Predicate] :=> t
  deriving (Eq)


instance Show t => Show (Qualified t) where
  show (preds :=> t) = "(" ++ intercalate ", " (map show preds ) ++ ") => " ++ show t
