module Compiler.Syntax.Class where


import Compiler.Syntax.Name
import Compiler.Syntax.Instance


type Supers = [Name]


type Class = (Supers, [Instance])
