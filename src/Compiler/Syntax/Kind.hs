module Compiler.Syntax.Kind where


import Compiler.Syntax.Name


data Kind
  = K'Star
  | K'Arr Kind Kind
  | K'Var Name
  deriving (Eq)


instance Show Kind where
  show K'Star = "*"
  show (K'Arr k'dom@(K'Arr _ _) k'codom) = "(" ++ show k'dom ++ ") -> " ++ show k'codom
  show (K'Arr k'dom k'codom) = show k'dom ++ " -> " ++ show k'codom
  show (K'Var name) = "*?" ++ name
