module Interpreter.Core where


import Data.List ( intercalate )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal ( Literal )
import Compiler.Syntax.Pattern ( Pattern )
import Interpreter.Error ( Evaluation'Error )


data Core
  = Var Name
  | Prim'Op Name
  | Lit Literal
  | Abs Name Core
  | Case Core [Match]
  | Intro Name [Core]
  | App Core Core
  | Tuple [Core]
  | Let [Binding] Core
  | Error Evaluation'Error -- errors like inexhaustive pattern matching
  deriving (Eq)


instance Show Core where
  show (Var name)
    = name

  show (Prim'Op name)
    = "(" ++ name ++ ")"

  show (Lit lit)
    = show lit

  show (Abs param body)
    = "(\\ " ++ param ++ " -> " ++ show body ++ ")"

  show (Case motive matches)
    = "case " ++ show motive ++ " of\n" ++ intercalate "\n" (map show matches) ++ "\n"

  show (Intro tag args)
    = "(Intro " ++ tag ++ " " ++ unwords (map show args) ++ ")"

  show (App left right@(App a'left a'right))
    = show left ++ " (" ++ show right ++ ")"

  show (App left right) = show left ++ " " ++ show right

  show (Tuple args)
    = "(" ++ intercalate ", " (map show args) ++ ")"

  show (Let binds expr)
    = "let " ++ show binds ++ " in " ++ show expr

  show (Error err) = "Error " ++ show err


data Binding = Binding { name :: Name, lambda :: Core }
  deriving (Show, Eq)


data Match = Match { patterns :: [Pattern], rhs :: Core }
  deriving (Show, Eq)
