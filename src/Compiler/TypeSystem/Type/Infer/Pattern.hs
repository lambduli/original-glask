module Compiler.TypeSystem.Type.Infer.Pattern where


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Pattern ( Pattern(..) )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V(T'V), Type(T'Var) )


import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Assumption ( Assumption )
import Compiler.TypeSystem.Type.Infer.Literal ( infer'lit )
import Compiler.TypeSystem.Utils.Infer ( to'scheme )


infer'pat :: Pattern -> Infer ([Predicate], Type, [Assumption Sigma'Type])
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


infer'pats :: [Pattern] -> Infer ([Predicate], [Type], [Assumption Sigma'Type])
infer'pats pats = do
  ps'ty'as's <- mapM infer'pat pats
  let preds       = concat  [preds'       | (preds' , _     , _           ) <- ps'ty'as's]
      types       =         [type'        | (_      , type' , _           ) <- ps'ty'as's]
      assumptions = concat  [assumptions' | (_      , _     , assumptions') <- ps'ty'as's]
  return (preds, types, assumptions)
