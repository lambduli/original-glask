{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Predicate where


import Compiler.Syntax.Name

import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Syntax.Term.Identifier

import Compiler.Syntax.Term.Type

data Term'Pred = Is'In Name Term'Type
  deriving (Eq)


instance Term Term'Id Term'Pred where
  free'vars (Is'In name term'type)
    = free'vars term'type
