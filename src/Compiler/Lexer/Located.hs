module Compiler.Lexer.Located where


import Compiler.Lexer.Position


class Located a where
  at :: a -> Position
