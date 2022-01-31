module Compiler.Analysis.TypeSystem.Binding where


import Compiler.Syntax
import Compiler.Analysis.TypeSystem.Solver.Substitutable (Subst)


newtype Implicit = Implicit Bind'Group
  deriving (Show)


data Explicit = Explicit Scheme Bind'Group
  deriving (Show)


{-  
  Scheme - Annotation from the Type Class definition
  Subst T'V Type - Singleton substitution mapping a Type Class parameter/variable to the Instance defined Type
  Bind'Group - The method implementation itself.
-}
data Method = Method Scheme (Subst T'V Type) Bind'Group
  deriving (Show)
