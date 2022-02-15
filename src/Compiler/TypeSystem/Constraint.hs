module Compiler.TypeSystem.Constraint where


data Constraint a
  = Unify a a


instance (Show a) => Show (Constraint a) where
  show (a `Unify` b) = " " ++ show a ++ " Unify " ++ show b ++ " "
