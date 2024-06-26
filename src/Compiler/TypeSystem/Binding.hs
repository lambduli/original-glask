module Compiler.TypeSystem.Binding where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.BindGroup ( Bind'Group )
import Compiler.Syntax.Type ( Sigma'Type )


newtype Implicit = Implicit Bind'Group
  deriving (Show)


data Explicit = Explicit Sigma'Type Bind'Group
  deriving (Show)


{-  
  Scheme - Annotation from the Type Class definition
  
  Subst T'V Type - Singleton substitution mapping a Type Class parameter/variable to the Instance defined Type
  -- Update: I think what I am going to do is I will not pass the Substitution itself
  -- I deem it better to apply the substitution to the Qualfied Type from the method annotation BEFORE I close over it and obtain Type Scheme.
  -- That way I won't need to close over the Class Parameter and later try to undo that in complicated and error prone way.

  Bind'Group - The method implementation itself.
-}
data Method = Method Sigma'Type {- (Subst T'V Type) -} Bind'Group Name Name -- type scheme, implementation, type class, global variable name where it will be put
  deriving (Show)
