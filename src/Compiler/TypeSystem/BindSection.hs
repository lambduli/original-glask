module Compiler.TypeSystem.BindSection where


import Compiler.TypeSystem.Binding ( Explicit, Implicit )


type Bind'Section = ([Explicit], [[Implicit]])
