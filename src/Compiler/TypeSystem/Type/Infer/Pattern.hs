module Compiler.TypeSystem.Type.Infer.Pattern where


import Compiler.Counter

import Compiler.Syntax
import Compiler.Syntax.Type


import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Assumption
import Compiler.TypeSystem.Type.Infer.Literal
import Compiler.TypeSystem.Utils.Infer


infer'pat :: Pattern -> Infer ([Predicate], Type, [Assumption Scheme])
infer'pat (P'Var name) = do
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  return ([], t'var, [(name, to'scheme t'var)])

infer'pat (P'Con name patterns) = do
  undefined
  -- TODO: continue here
  -- I think it's not going to be that complicated as the paper makes it seem
  -- type schemes for the constructors are also stored in the typing context

infer'pat (P'Lit lit) = do
  (preds, type') <- infer'lit lit
  return (preds, type', [])

infer'pat (P'As name pattern) = do
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  return ([], t'var, [(name, to'scheme t'var)])

infer'pat P'Wild = do
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  return ([], t'var, [])


infer'pats :: [Pattern] -> Infer ([Predicate], [Type], [Assumption Scheme])
infer'pats pats = do
  psasts <- mapM infer'pat pats
  let preds       = concat  [preds'       | (preds' , _     , _           ) <- psasts]
      types       =         [type'        | (_      , type' , _           ) <- psasts]
      assumptions = concat  [assumptions' | (_      , _     , assumptions') <- psasts]
  return (preds, types, assumptions)
