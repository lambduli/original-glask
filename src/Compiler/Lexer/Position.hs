module Compiler.Lexer.Position where


data Position
  = Position  { line :: Int, column :: Int }
  | None
  deriving (Eq, Show)
