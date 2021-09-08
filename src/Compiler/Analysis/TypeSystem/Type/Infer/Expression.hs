module Compiler.Analysis.TypeSystem.Type.Infer.Expression where


import Control.Monad


import Compiler.Syntax
import Compiler.Syntax.Expression

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Type.Constants

import Compiler.Analysis.TypeSystem.Type.Infer.Literal
import Compiler.Analysis.TypeSystem.Type.Infer.Pattern

import Compiler.Analysis.TypeSystem.Utils.Infer



infer'expr :: Expression -> Infer ([Predicate], Type, [Constraint Type], [Constraint Kind])
infer'expr (Var var'name) = do
  preds :=> type' <- lookup't'env var'name
  return (preds, type', [], [])

infer'expr (Const const'name) = do
  preds :=> type' <- lookup't'env const'name
  return (preds, type', [], [])

infer'expr (Op op'name) = do
  preds :=> type' <- lookup't'env op'name
  return (preds, type', [], [])

infer'expr (Lit lit) = do
  (preds, type') <- infer'lit lit
  return (preds, type', [], [])

infer'expr (Abs pattern'param body) = do
  (preds'param, type'param, assumptions'param) <- infer'pat pattern'param
  (preds'body, type'body, t'constrs, k'constrs) <- merge'into't'env assumptions'param (infer'expr body)
  return (preds'param ++ preds'body, type'param `type'fn` type'body, t'constrs, k'constrs)
  
  -- TODO: I need to rewrite this
  -- I will need to find out how to infer'expr types for patterns
  -- then infer'expr the type of that pattern - which could also bind some variables to types
  -- this needs to come together with the result - so infer'expr on Pattern will also give the bindings
  -- so that I can put them inside the t'env and infer'expr the type of the body
  -- fresh'name <- fresh
  -- let t'var = T'Var (T'V fresh'name K'Star)
  -- (preds, type', t'constrs, k'constrs) <- put'in't'env (x, For'All [] ([] :=> t'var)) (infer'expr body)
  -- return (preds, t'var `type'fn` type', t'constrs, k'constrs)

-- TODO: check if it's really valid
infer'expr (App left right) = do
  (preds'l, t'l, cs'l, k'cs'l) <- infer'expr left
  (preds'r, t'r, cs'r, k'cs'r) <- infer'expr right
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  return (preds'l ++ preds'r, t'var, cs'l ++ cs'r ++ [t'l `Unify` (t'r `type'fn` t'var)], k'cs'l ++ k'cs'r)

infer'expr (Infix'App left op right) = do
  undefined
  -- TODO: implement later - should be easy enough

-- TODO: check if it's really valid
infer'expr (Tuple exprs) = do
  (preds, types, cs, k'cs) <- foldM infer' ([], [], [], []) exprs
  return (preds, T'Tuple $ reverse types, cs, k'cs)
    where
      infer' (preds, types, constrs, k'constrs) expr = do
        (preds, t, cs, k'cs) <- infer'expr expr
        return (preds, t : types, cs ++ constrs, k'cs ++ k'constrs)

-- TODO: check if it's really valid
infer'expr (If condition then' else') = do
    (preds'cond, t1, c1, k'c1) <- infer'expr condition
    (preds'tr, t2, c2, k'c2) <- infer'expr then'
    (preds'fl, t3, c3, k'c3) <- infer'expr else'
    return (preds'cond ++ preds'tr ++ preds'fl, t2, (t1 `Unify` t'Bool) : (t2 `Unify` t3) : c1 ++ c2 ++ c3, k'c1 ++ k'c2 ++ k'c3)

infer'expr (Let decls body) = do
  undefined

-- | TODO: I need to read a section about type checking explicitly annotated bindings
-- |        from that I think I should be able to derive the plan for this specific case.
infer'expr (Ann expr qual'type) = do
  undefined

-- | TODO: I will need to think about how to translate the case expression into Let
--          maybe it's because the THIH mentions a Let having a type Bind'Group -> Expression
--          so maybe the trick is done by the Bind'Group part - I am not sure.
--          Once I figure that out I should be able to derive the strategy for the case expression.
infer'expr (Case expr matches) = do
  undefined

-- | TODO: Introductors (how I call them) already have the type annotation in the typing context/
--          So all I need is to retrieve it from there.
infer'expr (Intro name exprs) = do
  undefined
