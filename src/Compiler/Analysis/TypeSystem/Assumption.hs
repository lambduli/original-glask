module Compiler.Analysis.TypeSystem.Assumption where


import Compiler.Syntax



{-  Assumption represents a assignment of a Scheme or Kind (in most cases) to a variable name.
    The first part is represented as a Name.
    NOTE: It must be a Name (String) and not a T'V because then the lookup woudl get awkward
    since the T'V also contains a value of type Kind and as it is going to be mostly K'Var _
    I would need to ignore it anyway - so why bother.
-}
type Assumption a = (Name, a)
