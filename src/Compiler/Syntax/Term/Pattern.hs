module Compiler.Syntax.Term.Pattern where

import Data.List.Extra (intercalate)


import Compiler.Syntax.Name
import Compiler.Syntax.Literal
import Compiler.Syntax.Term.Identifier


data Term'Pat
  = Term'P'Id Term'Id
  -- | Term'P'Const Term'Id
  | Term'P'Op Term'Id
  | Term'P'Lit Literal
  | Term'P'App [Term'Pat]
  | Term'P'Labeled Name [(Name, Term'Pat)] -- can be desugared to Term'P'App with correct order of field values
  | Term'P'Tuple [Term'Pat] -- will be able to desugar Term'P'App
  | Term'P'List [Term'Pat] -- will be able to desugar - same way
  | Term'P'As Name Term'Pat -- named pattern
  | Term'P'Wild

  | Term'P'Con Name [Term'Pat]
  deriving (Eq)


instance Show Term'Pat where
  show (Term'P'Id term'id) =
    show term'id

  show (Term'P'Op term'id) =
    show term'id

  show (Term'P'Lit lit) =
    show lit

  show (Term'P'App term'patterns) = unwords $ map show term'patterns

  show (Term'P'Labeled con'name field'patterns) =
    let
      show'field'patterns (name, term'pat) = name ++ " = " ++ show term'pat
    in
      con'name ++ "{ " ++ unwords (map show'field'patterns field'patterns) ++ " }"

  show (Term'P'Tuple term'patterns) =
    "(" ++ intercalate ", " (map show term'patterns) ++ ")"

  show (Term'P'List term'patterns) = show term'patterns

  show (Term'P'As name term'pat) =
    name ++ "@" ++ show term'pat

  show Term'P'Wild =
    "_"
