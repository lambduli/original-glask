module Compiler.TypeSystem.Class where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import Compiler.Syntax.Instance


type Supers = [Name]


type Class = (Supers, [Instance])
