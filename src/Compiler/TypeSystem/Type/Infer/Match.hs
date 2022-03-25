{-# LANGUAGE PatternSynonyms #-}

module Compiler.TypeSystem.Type.Infer.Match where


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

import Debug.Trace
import Compiler.Syntax.Pattern (Pattern)
import Compiler.TypeSystem.Assumption (Assumption)


infer'pats' :: [Pattern] -> Expected Type -> Type'Check ([Predicate], [Type], [Assumption Sigma'Type], Type)
infer'pats' [] Infer = do
  meta <- fresh'meta
  return ([], [], [], meta)

infer'pats' [] (Check t) = do
  return ([], [], [], t)

infer'pats' (pat : pats) expected = do
  ty' <- case expected of
              Check t -> return t
              Infer -> do
                fresh'meta
  
  (arg'ty, res'ty) <- unify'fun ty'
  
  (preds, assumptions) <- check'pattern pat arg'ty

  (preds', types', assumptions', last'type) <- infer'pats' pats (Check res'ty)

  return (preds ++ preds', arg'ty : types', assumptions ++ assumptions', last'type)

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


infer'match :: Match -> Expected Type -> Type'Check (Actual Type, [Predicate], [Predicate])
infer'match match@Match{ patterns = patterns, rhs = expr } expected = do
  -- ([Predicate], [Type], [Assumption Scheme])
  (preds'patts, types'patts, as'patts, rhs'type) <- infer'pats' patterns expected
  -- now I have the first and the third value to return
  -- I can use the list of assumptions to infer the body
  -- ([Predicate], Type, [Constraint Type], [Constraint Kind])
  preds'expr <- merge'into't'env as'patts (check'rho expr rhs'type)
  -- now I have the second, fourth, fifth, and sixth
  
  let actual'type = case expected of
                      Check _ -> Checked
                      Infer -> Inferred $ foldr T'Fun rhs'type types'patts

  return (actual'type, preds'patts, preds'expr)
  -- TODO: NOTE
  -- It is not really necessary to return the predicates in two distinct lists.
  -- I think they could be merged for good.


-- TODO: Ja si myslim, ze tuhle funkci budu muset prepsat tak, ze kazdy mode ma svoji equation

-- v checking modu, dostanu primo typ jako argument - typ funkce - typy patternu jako vstupy
-- typ prave strany jako typ vystupu
--
-- dava pak teda smysl, abych vzal ten type a poslal ho do infer'match
-- pak uz neni potreba abych tady delal nejakou unifikaci
-- protoze bych se mel moct spolehnout na to, ze pokud bude nejaka potreba, tak se provede dole
-- diky infer'pats a infer'expr
infer'matches :: [Match] -> Expected Type -> Type'Check ([Predicate], Actual Type)
infer'matches matches expected@(Check type') = do
  -- results :: [([Type], Type, [Predicate], [Predicate])]
  results <- mapM (`infer'match` expected) matches

  -- I think I don't need that anymore
  -- let types       =         [ foldr type'fn t'expr ts'patts | (ts'patts, t'expr, _, _, _) <- results ]
  -- Previous line makes a function types from the [Type] and Type in the results.
  -- let cs'unif     = map (Unify type') types
  -- Previous line unifies each function type with the type' passed as an argument.

  let preds'patts = concat  [ pred  | (_, pred, _) <- results ]
  let preds'exprs = concat  [ pred  | (_, _, pred) <- results ]
  let preds       = preds'patts ++ preds'exprs


  return (preds, Checked)

infer'matches matches Infer = do
  -- results :: [([Type], Type, [Predicate], [Predicate])]
  results <- mapM (`infer'match` Infer) matches

  -- let types = [ foldr (type'fn . from'inferred) t'expr actual'ts'patts | (actual'ts'patts, Inferred t'expr, _, _) <- results ]
  let types = [ type' | (Inferred type', _, _) <- results ]

  meta'var <- fresh'meta -- Musel jsem si vyrobit sam promennou, pres kterou muzu spojit vsechny typy vraceny ze vsech infer'match
  
  -- BIG TODO: Tohle je takovej pokus, pokud je vsechno spravne, tak muze tahle promenna byt uplne normalni Tau promenna a bude to porad fungovat
  -- fr'name <- fresh
  -- let meta'var = T'Meta (T'S fr'name K'Star)
  
  let cs'unif     = map (Unify meta'var) types
  add'constraints cs'unif

  -- Previous line unifies all function types together. 

  let preds'patts = concat  [ pred  | (_, pred, _) <- results ]
  let preds'exprs = concat  [ pred  | (_, _, pred) <- results ]
  let preds       = preds'patts ++ preds'exprs

  -- let res'type = head [ t | (Inferred t, _, _) <- results ]
  -- let type' = foldr T'Fun res'type $ map (\ (Inferred t) -> t) $ head [ actual'types | (actual'types, _, _, _) <- results ]

  return (preds, Inferred meta'var)

    -- where
      -- from'inferred (Inferred t) = t
      -- NOTE: This function is partial - it should be always OK
      -- if it fails, that means there's an bug somewhere in the implementation
      -- and even though the inference was given a command to `Infer` that type, it for some reason
      -- returned `Checked` - which must never happen
