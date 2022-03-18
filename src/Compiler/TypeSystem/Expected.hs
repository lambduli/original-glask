module Compiler.TypeSystem.Expected where


data Expected t = Check t
                | Infer
                deriving (Eq, Show)

