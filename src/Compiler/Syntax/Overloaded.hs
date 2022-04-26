module Compiler.Syntax.Overloaded where


import Compiler.Syntax.Name (Name)


data Overloaded = Overloaded
                | Method Name -- name of the type class this method belongs into
                | Recursive
                deriving (Show, Eq)
