{-# LANGUAGE PatternSynonyms #-}

module Compiler.TypeSystem.Type.Infer.Match where


import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind (K'Star) )
import Compiler.Syntax.Match ( Match(..) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Type ( Type (T'Meta), Sigma'Type )

import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'constraints )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Expression ( infer'expr )
import Compiler.TypeSystem.Type.Infer.Pattern ( infer'pats, infer'pat, check'pattern )
import Compiler.TypeSystem.Utils.Infer ( merge'into't'env, fresh'meta, check'rho, unify'fun )
import Compiler.TypeSystem.Type.Constants ( type'fn )

import Compiler.TypeSystem.Expected ( Expected (Check, Infer) )
import Compiler.TypeSystem.Actual ( Actual(Checked, Inferred) )
import Compiler.Syntax.TFun ( pattern T'Fun )
import Compiler.Syntax.Pattern ( Pattern )
import Compiler.TypeSystem.Assumption ( Assumption )
import Compiler.TypeSystem.Error ( Error(Unexpected) )


infer'pats' :: [Pattern] -> Expected Type -> Type'Check ([Pattern], [Predicate], [Type], [Assumption Sigma'Type], Type)
infer'pats' [] Infer = do
  meta <- fresh'meta
  return ([], [], [], [], meta)

infer'pats' [] (Check t) = do
  return ([], [], [], [], t)

infer'pats' (pat : pats) expected = do
  ty' <- case expected of
              Check t -> return t
              Infer -> do
                fresh'meta
  
  (arg'ty, res'ty) <- unify'fun ty'
  
  (pat', preds, assumptions) <- check'pattern pat arg'ty

  (pats', preds', types', assumptions', last'type) <- infer'pats' pats (Check res'ty)

  return (pat' : pats', preds ++ preds', arg'ty : types', assumptions ++ assumptions', last'type)

  -- tahle funkce by mela postupne pomoci unify'fun (nebo tak neco)
  -- rozlozit ten expected type na argument type a result type
  -- a v zasade vzdycky pro kazdej pattern potrebuju ziskat ten arg-type abych mohl ten pattern infernout pomoci toho typu
  -- to je to volani infer'pat - a nemusim uz pak volat infer'pat ale asi bych mohl rovnou volat nejakej ten check'pattern
  -- 
  -- predikaty dostanu snad z check'pattern
  -- list typu odpovidajici typum patternu si vyrobim pri tom unify'fun-ovani - to budou ty typy argumentu
  -- assumption sigma - to dostanu taky z check'pattern
  -- type - to je typ, kterej mi vrati posledni volani unify'fun jako result type

  -- ps'ty'as's <- mapM (`infer'pat` expected) pats
  -- let preds       = concat  [preds        | (preds  , _     , _           ) <- ps'ty'as's]
  --     types       =         [type'        | (_      , type' , _           ) <- ps'ty'as's]
  --     assumptions = concat  [assumptions  | (_      , _     , assumptions ) <- ps'ty'as's]

  -- return (preds, types, assumptions, undefined)


infer'match :: Match -> Expected Type -> Type'Check (Match, Actual Type, [Predicate], [Predicate])
infer'match match@Match{ patterns = patterns, rhs = expr } expected = do
  -- ([Predicate], [Type], [Assumption Scheme])
  (patterns', preds'patts, types'patts, as'patts, rhs'type) <- infer'pats' patterns expected
  -- now I have the first and the third value to return
  -- I can use the list of assumptions to infer the body
  -- ([Predicate], Type, [Constraint Type], [Constraint Kind])
  (expr', preds'expr) <- merge'into't'env as'patts (check'rho expr rhs'type)
  -- now I have the second, fourth, fifth, and sixth
  
  let actual'type = case expected of
                      Check _ -> Checked
                      Infer -> Inferred $ foldr T'Fun rhs'type types'patts

  return (match{ patterns = patterns', rhs = expr' }, actual'type, preds'patts, preds'expr)
  -- TODO: NOTE
  -- It is not really necessary to return the predicates in two distinct lists.
  -- I think they could be merged for good.


{-  NOTE: This function is an abstraction on top of tc'matches -}
check'matches :: [Match] -> Type -> Type'Check ([Match], [Predicate])
check'matches matches type' = do
  (matches', preds, actual) <- tc'matches matches (Check type')
  case actual of
    Checked -> do
      return (matches', preds)
    Inferred ty -> do
      throwError $ Unexpected "Internal Error while 'check'matches'"


infer'matches :: [Match] -> Type'Check ([Match], [Predicate], Type)
infer'matches matches = do
  (matches', preds, actual) <- tc'matches matches (Infer)
  case actual of
    Checked -> do
      throwError $ Unexpected "Internal Error while 'infer'matches'"
    Inferred ty -> do
      return (matches', preds, ty)


tc'matches :: [Match] -> Expected Type -> Type'Check ([Match], [Predicate], Actual Type)
tc'matches matches expected@(Check type') = do
  -- results :: [([Type], Type, [Predicate], [Predicate])]
  results <- mapM (`infer'match` expected) matches

  let matches'    = [ match | (match, _, _, _) <- results ]
  let preds'patts = concat  [ pred  | (_, _, pred, _) <- results ]
  let preds'exprs = concat  [ pred  | (_, _, _, pred) <- results ]
  let preds       = preds'patts ++ preds'exprs

  return (matches', preds, Checked)

tc'matches matches Infer = do
  -- results :: [([Type], Type, [Predicate], [Predicate])]
  results <- mapM (`infer'match` Infer) matches

  -- let types = [ foldr (type'fn . from'inferred) t'expr actual'ts'patts | (actual'ts'patts, Inferred t'expr, _, _) <- results ]
  let types = [ type' | (_, Inferred type', _, _) <- results ]

  meta'var <- fresh'meta -- Musel jsem si vyrobit sam promennou, pres kterou muzu spojit vsechny typy vraceny ze vsech infer'match
  
  let cs'unif     = map (Unify meta'var) types
  add'constraints cs'unif

  -- Previous line unifies all function types together. 

  let matches'    = [ match | (match, _, _, _) <- results ]
  let preds'patts = concat  [ pred  | (_, _, pred, _) <- results ]
  let preds'exprs = concat  [ pred  | (_, _, _, pred) <- results ]
  let preds       = preds'patts ++ preds'exprs

  return (matches', preds, Inferred meta'var)
