module Compiler.Analysis.TypeSystem.Constraint where


data Constraint a
  = Unify a a
