module Compiler.TypeSystem.Type.Infer.Match where


import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Match ( Match(..) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Type ( Type )

import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Expression ( infer'expr )
import Compiler.TypeSystem.Type.Infer.Pattern ( infer'pats )
import Compiler.TypeSystem.Utils.Infer ( merge'into't'env )
import Compiler.TypeSystem.Type.Constants ( type'fn )


infer'match :: Match -> Infer ([Type], Type, [Predicate], [Predicate], [Constraint Type])
infer'match Match{ patterns = patterns, rhs = expr } = do
  -- ([Predicate], [Type], [Assumption Scheme])
  (preds'patts, types'patts, as'patts) <- infer'pats patterns
  -- now I have the first and the third value to return
  -- I can use the list of assumptions to infer the body
  -- ([Predicate], Type, [Constraint Type], [Constraint Kind])
  (preds'expr, type'expr, t'cs) <- merge'into't'env as'patts (infer'expr expr)
  -- now I have the second, fourth, fifth, and sixth
  return (types'patts, type'expr, preds'patts, preds'expr, t'cs)
  -- TODO: NOTE
  -- It is not really necessary to return the predicates in two distinct lists.
  -- I think they could be merged for good.


infer'matches :: [Match] -> Type -> Infer ([Predicate], [Constraint Type])
infer'matches matches type' = do
  -- results :: [([Type], Type, [Predicate], [Predicate], [Constraint Type])]
  results <- mapM infer'match matches

  let types       =         [ foldr type'fn t'expr ts'patts | (ts'patts, t'expr, _, _, _) <- results ]
  -- Previous line makes a function types from the [Type] and Type in the results.
  let cs'unif     = map (Unify type') types
  -- Previous line unifies each function type with the type' passed as an argument.

  let preds'patts = concat  [ pred  | (_, _, pred, _, _) <- results ]
  let preds'exprs = concat  [ pred  | (_, _, _, pred, _) <- results ]
  let preds       = preds'patts ++ preds'exprs

  let cs't        = cs'unif ++ concat  [ cs    | (_, _, _, _, cs)   <- results ]

  return (preds, cs't)
