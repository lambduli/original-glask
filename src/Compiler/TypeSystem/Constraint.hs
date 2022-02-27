module Compiler.TypeSystem.Constraint where


data Constraint a
  = Unify a a
  | Match a a


instance (Show a) => Show (Constraint a) where
  show (a `Unify` b)
    = " " ++ show a ++ " Unify " ++ show b ++ " "

  show (a `Match` b)
    = " " ++ show a ++ " Match " ++ show b ++ " "

