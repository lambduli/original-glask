module Interpreter.Error where

import Compiler.Syntax.Name ( Name )

import Interpreter.Value ( Value )


data Evaluation'Error
  = Unbound'Var Name
  | Bad'Operator'Application String Value
  | Index'Out'Of'Bound Int
  | Nil'Head'Exception
  | Nil'Tail'Exception
  | Empty'String'Exception
  | Division'By'Zero Int
  | Unexpected String
  | Non'Exhaustive


instance Show Evaluation'Error where
  show (Unbound'Var name) =
    "Unknown variable " ++ name
  show (Bad'Operator'Application name exp) =
    "Bad use of the operator " ++ name ++ "\n  in the expression \n    (" ++ name ++ ")" -- show exp ++
  show (Index'Out'Of'Bound ind) =
    "Index out of the bound error. (" ++ show ind ++ ")"
  show Nil'Head'Exception =
    "Native function #head called on an empty list."
  show Nil'Tail'Exception =
    "Native function #tail called on an empty list."
  show Empty'String'Exception =
    "Operation called with an empty string."
  show (Division'By'Zero left) =
    "Division by zero error. (" ++ show left ++ " / 0)"
  show (Unexpected message) =
    "Unexpected: " ++ message
  show Non'Exhaustive =
    "Non-exhaustive patterns"
