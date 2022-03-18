module Compiler.TypeSystem.Actual where


data Actual t = Checked
              | Inferred t
              deriving (Show)
