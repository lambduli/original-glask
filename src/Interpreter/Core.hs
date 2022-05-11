module Interpreter.Core where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal ( Literal )
import Compiler.Syntax.Pattern ( Pattern )
import Interpreter.Error ( Evaluation'Error )
import Data.List (intercalate)


data Core
  = Var Name
  | Const Name
  | Op Name
  | Lit Literal
  | Abs Name Core
  | Case Core [Match]
  | Intro Name [Core]
  | App Core Core
  | Tuple [Core]
  | Let [Binding] Core
  | Error Evaluation'Error -- errors like inexhaustive pattern matching


instance Show Core where
  show (Var name) = name
  show (Const name) = name
  show (Op name) = "(" ++ name ++ ")"
  show (Lit lit) = show lit
  show (Abs param body) = "(\\ " ++ param ++ " -> " ++ show body ++ ")"
  show (Case motive matches) = "case " ++ show motive ++ " of\n" ++ (intercalate "\n" $ map show matches) ++ "\n"
  show (Intro tag args) = "(Intro " ++ tag ++ " " ++ (unwords $ map show args) ++ ")"
  show (App left right) = "( ( " ++ show left ++ ") " ++ show right ++ " ) )"
  show (Tuple args) = "Tuple " ++ (intercalate ", " $ map show args)
  show (Let binds expr) = "let " ++ show binds ++ " in " ++ show expr
  show (Error err) = "Error"

  -- | Infix'App Expression Expression Expression
  -- | If Expression Expression Expression
  -- | Ann Expression Sigma'Type
  -- | Hole Name
  -- | Placeholder Placeholder


data Binding = Binding { name :: Name, lambda :: Core }
  deriving (Show)


-- data Bind'Group = Bind'Group { name :: Name, lambda :: Core } -- name of the function and a lambda completely translated into the core


data Match = Match { patterns :: [Pattern], rhs :: Core }
  deriving (Show)