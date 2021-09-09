module Compiler.Analysis.TypeSystem.Type.Infer.Match where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint

import {-# SOURCE #-} Compiler.Analysis.TypeSystem.Type.Infer.Expression
import Compiler.Analysis.TypeSystem.Type.Infer.Pattern

import Compiler.Analysis.TypeSystem.Utils.Infer


infer'match :: Match -> Infer ([Type], Type, [Predicate], [Predicate], [Constraint Type], [Constraint Kind])
infer'match Match{ patterns = patterns, rhs = expr } = do
  -- ([Predicate], [Type], [Assumption Scheme])
  (preds'patts, types'patts, as'patts) <- infer'pats patterns
  -- now I have the first and the third value to return
  -- I can use the list of assumptions to infer the body
  -- ([Predicate], Type, [Constraint Type], [Constraint Kind])
  (preds'expr, type'expr, t'cs, k'cs) <- merge'into't'env as'patts (infer'expr expr)
  -- now I have the second, fourth, fifth, and sixth
  return (types'patts, type'expr, preds'patts, preds'expr, t'cs, k'cs)
