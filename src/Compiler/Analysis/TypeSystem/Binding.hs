module Compiler.Analysis.TypeSystem.Binding where


import Compiler.Syntax


newtype Implicit = Implicit Bind'Group


data Explicit = Explicit Scheme Bind'Group
