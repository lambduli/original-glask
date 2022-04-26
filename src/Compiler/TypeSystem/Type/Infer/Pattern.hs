module Compiler.TypeSystem.Type.Infer.Pattern where


import Control.Monad.Except ( MonadError(throwError) )
import Compiler.TypeSystem.Error ( Error(Unexpected) )


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Pattern ( Pattern(..) )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )


import Compiler.TypeSystem.Infer ( Infer, Type'Check )
import Compiler.TypeSystem.Assumption ( Assumption )
import Compiler.TypeSystem.Type.Infer.Literal ( infer'lit )
import Compiler.TypeSystem.Utils.Infer ( to'scheme, fresh'meta, subs'check, lookup't'env, instantiate, split'data'cons )
import Compiler.TypeSystem.Expected ( Expected(Infer, Check) )
import Compiler.TypeSystem.Actual ( Actual(Checked, Inferred) )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'specify )


infer'pat :: Pattern -> Expected Sigma'Type -> Type'Check (Pattern, [Predicate], Actual Type, [Assumption Sigma'Type])
-- PTIART
infer'pat p@(P'Var name) Infer = do
  meta'var <- fresh'meta
  return (p, [], Inferred meta'var, [(name, to'scheme meta'var)])
  -- fresh'name <- fresh
  -- let t'var = T'Var (T'V fresh'name K'Star)
  -- return ([], t'var, [(name, to'scheme t'var)])

-- PTIART
infer'pat p@(P'Var name) (Check sigma) = do
  return (p, [], Checked, [(name, to'scheme sigma)])
  -- fresh'name <- fresh
  -- let t'var = T'Var (T'V fresh'name K'Star)
  -- return ([], t'var, [(name, to'scheme t'var)])

-- PTIART
infer'pat (P'Con name patterns) expected = do
  sigma <- lookup't'env name expected
  _ :=> rho <- instantiate sigma -- NOTE: the context should always be empty, since we can't qualify constructors - it's safe to ignore it then
  -- TODO: Now I need to take `rho` and split it into a list of argument types and a resulting type
  let (arg'types, res'type) = split'data'cons rho
  
  results <- mapM check'arg (patterns `zip` arg'types)
  let patterns'   = [ pattern | (pattern, _, _) <- results ]
  let preds       = concat [ preds       | ( _, preds , _          ) <- results ]
      assumptions = concat [ assumptions | ( _, _     , assumptions) <- results ]

  (preds', actual) <- inst'pat'sigma res'type expected

  return (P'Con name patterns', preds ++ preds', actual, assumptions)
    where
      check'arg (p, p't) = check'pattern p p't

-- PTIART
infer'pat p@(P'Lit lit) expected = do
  (preds, type') <- infer'lit lit expected
  return (p, preds, type', [])

-- PTIART
infer'pat (P'As name pattern) Infer = do
  (pattern', preds, type', assumptions) <- infer'pattern pattern
  return (P'As name pattern', preds, Inferred type', (name, to'scheme type') : assumptions)

-- PTIART
infer'pat (P'As name pattern) (Check type') = do
  (pattern', preds, assumptions) <- check'pattern pattern type'
  return (P'As name pattern', preds, Checked, (name, to'scheme type') : assumptions)

-- PTIART
infer'pat P'Wild Infer = do
  meta'var <- fresh'meta
  return (P'Wild, [], Inferred meta'var, [])

-- PTIART
infer'pat P'Wild (Check _) = do
  return (P'Wild, [], Checked, [])

-- PTIART
infer'pat (P'Ann pattern sigma) expected = do
  -- TODO: fully specify kinds within types in the `sigma`
  sigma' <- kind'specify sigma
  (pattern', preds, assumptions) <- check'pattern pattern sigma'
  (preds', actual) <- inst'pat'sigma sigma' expected
  return (P'Ann pattern' sigma, preds ++ preds', actual, assumptions)


infer'pats :: [Pattern] -> Expected Type -> Type'Check ([Pattern], [Predicate], [Actual Type], [Assumption Sigma'Type])
infer'pats pats expected = do
  ps'ty'as's <- mapM (`infer'pat` expected) pats
  let patterns    =         [ pat         | (pat, _, _ , _) <- ps'ty'as's]
      preds       = concat  [preds        | (_, preds  , _     , _           ) <- ps'ty'as's]
      types       =         [type'        | (_, _      , type' , _           ) <- ps'ty'as's]
      assumptions = concat  [assumptions  | (_, _      , _     , assumptions ) <- ps'ty'as's]

  return (patterns, preds, types, assumptions)


infer'pattern :: Pattern -> Type'Check (Pattern, [Predicate], Type, [Assumption Sigma'Type])
infer'pattern pattern = do
  result <- infer'pat pattern Infer
  case result of
    (pattern', preds, Inferred type', assumptions) -> do
      return (pattern', preds, type', assumptions)
    _ -> throwError $ Unexpected "Internal error while 'infer'pattern'."


check'pattern :: Pattern -> Sigma'Type -> Type'Check (Pattern, [Predicate], [Assumption Sigma'Type])
check'pattern pattern sigma = do
  result <- infer'pat pattern (Check sigma)
  case result of
    (pattern', preds, Checked, assumptions) -> do
      return (pattern', preds, assumptions)
    _ -> throwError $ Unexpected "Internal error while 'check'pattern'."


inst'pat'sigma :: Sigma'Type -> Expected Sigma'Type -> Type'Check ([Predicate], Actual Sigma'Type)
inst'pat'sigma pattern'type Infer = do
  return ([], Inferred pattern'type)

inst'pat'sigma pat_ty (Check exp_ty) = do
  preds <- subs'check exp_ty pat_ty
  return (preds, Checked)
