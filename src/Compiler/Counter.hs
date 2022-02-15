module Compiler.Counter where


newtype Counter = Counter{ counter :: Int }
  deriving (Show)