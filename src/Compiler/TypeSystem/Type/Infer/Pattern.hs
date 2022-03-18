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


infer'pat :: Pattern -> Expected Sigma'Type -> Type'Check ([Predicate], Actual Type, [Assumption Sigma'Type])
-- PTIART
infer'pat (P'Var name) Infer = do
  meta'var <- fresh'meta
  return ([], Inferred meta'var, [(name, to'scheme meta'var)])
  -- fresh'name <- fresh
  -- let t'var = T'Var (T'V fresh'name K'Star)
  -- return ([], t'var, [(name, to'scheme t'var)])

-- PTIART
infer'pat (P'Var name) (Check sigma) = do
  return ([], Checked, [(name, to'scheme sigma)])
  -- fresh'name <- fresh
  -- let t'var = T'Var (T'V fresh'name K'Star)
  -- return ([], t'var, [(name, to'scheme t'var)])

-- PTIART
infer'pat (P'Con name patterns) expected = do
  sigma <- lookup't'env name
  _ :=> rho <- instantiate sigma -- NOTE: the context should always be empty, since we can't qualify constructors - it's safe to ignore it then
  -- TODO: Now I need to take `rho` and split it into a list of argument types and a resulting type
  let (arg'types, res'type) = split'data'cons rho
  
  results <- mapM check'arg (patterns `zip` arg'types)
  let preds       = concat [ preds       | ( preds , _          ) <- results ]
      assumptions = concat [ assumptions | ( _     , assumptions) <- results ]

  (preds', actual) <- inst'pat'sigma res'type expected

  return (preds ++ preds', actual, assumptions)
    where
      check'arg (p, p't) = check'pattern p p't

-- PTIART
infer'pat (P'Lit lit) expected = do
  (preds, type') <- infer'lit lit expected
  return (preds, type', [])

-- PTIART
infer'pat (P'As name pattern) Infer = do
  (preds, type', assumptions) <- infer'pattern pattern
  return (preds, Inferred type', (name, to'scheme type') : assumptions)

-- PTIART
infer'pat (P'As name pattern) (Check type') = do
  (preds, assumptions) <- check'pattern pattern type'
  return (preds, Checked, (name, to'scheme type') : assumptions)

-- PTIART
infer'pat P'Wild Infer = do
  meta'var <- fresh'meta
  return ([], Inferred meta'var, [])

-- PTIART
infer'pat P'Wild (Check _) = do
  return ([], Checked, [])


infer'pats :: [Pattern] -> Expected Type -> Type'Check ([Predicate], [Actual Type], [Assumption Sigma'Type])
infer'pats pats expected = do
  ps'ty'as's <- mapM (`infer'pat` expected) pats
  let preds       = concat  [preds        | (preds  , _     , _           ) <- ps'ty'as's]
      types       =         [type'        | (_      , type' , _           ) <- ps'ty'as's]
      assumptions = concat  [assumptions  | (_      , _     , assumptions ) <- ps'ty'as's]

  return (preds, types, assumptions)


infer'pattern :: Pattern -> Type'Check ([Predicate], Type, [Assumption Sigma'Type])
infer'pattern pattern = do
  result <- infer'pat pattern Infer
  case result of
    (preds, Inferred type', assumptions) -> do
      return (preds, type', assumptions)
    _ -> throwError $ Unexpected "Internal error while 'infer'pattern'."


check'pattern :: Pattern -> Sigma'Type -> Type'Check ([Predicate], [Assumption Sigma'Type])
check'pattern pattern sigma = do
  result <- infer'pat pattern (Check sigma)
  case result of
    (preds, Checked, assumptions) -> do
      return (preds, assumptions)
    _ -> throwError $ Unexpected "Internal error while 'check'pattern'."


inst'pat'sigma :: Sigma'Type -> Expected Sigma'Type -> Type'Check ([Predicate], Actual Sigma'Type)
inst'pat'sigma pattern'type Infer = do
  return ([], Inferred pattern'type)

inst'pat'sigma pat_ty (Check exp_ty) = do
  preds <- subs'check exp_ty pat_ty
  return (preds, Checked)
