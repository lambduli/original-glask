module Compiler.Analysis.TypeSystem.Class where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import Compiler.Syntax.Instance


type Supers = [Name]


-- TODO: Question:  Should I also add a `Name` parameter denoting the name of the class type variable?
--                  I am going to need it for method elaboration - to construct the trivial substitution between that exact variable and the Instance Type.
type Class = (Supers, [Instance])
