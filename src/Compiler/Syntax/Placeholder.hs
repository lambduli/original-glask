module Compiler.Syntax.Placeholder where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Type ( Type )


data Placeholder  = Dictionary Name Type
                  | Method Name Type Name -- name of the method, type, name of the type class
                  | Recursive Name Type
                  deriving (Show, Eq)
