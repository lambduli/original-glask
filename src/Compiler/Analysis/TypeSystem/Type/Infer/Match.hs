module Compiler.Analysis.TypeSystem.Type.Infer.Match where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint

import {-# SOURCE #-} Compiler.Analysis.TypeSystem.Type.Infer.Expression
import Compiler.Analysis.TypeSystem.Type.Infer.Pattern

import Compiler.Analysis.TypeSystem.Utils.Infer

import Compiler.Analysis.TypeSystem.Type.Constants


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
  -- TODO: NOTE
  -- It is not really necessary to return the predicates in two distinct lists.
  -- I think they could be merged for good.


infer'matches :: [Match] -> Type -> Infer ([Predicate], [Constraint Type], [Constraint Kind])
infer'matches matches type' = do
  -- results :: [([Type], Type, [Predicate], [Predicate], [Constraint Type], [Constraint Kind])]
  results <- mapM infer'match matches

  let types       =         [ foldr type'fn t'expr ts'patts | (ts'patts, t'expr, _, _, _, _) <- results ]
  -- Previous line makes a function types from the [Type] and Type in the results.
  let cs'unif     = map (Unify type') types
  -- Previous line unifies each function type with the type' passed as an argument.

  let preds'patts = concat  [ pred  | (_, _, pred, _, _, _) <- results ]
  let preds'exprs = concat  [ pred  | (_, _, _, pred, _, _) <- results ]
  let preds       = preds'patts ++ preds'exprs

  let cs't        = cs'unif ++ concat  [ cs    | (_, _, _, _, cs, _)   <- results ]
  let cs'k        = concat  [ cs    | (_, _, _, _, _, cs)   <- results ]

  return (preds, cs't, cs'k)
