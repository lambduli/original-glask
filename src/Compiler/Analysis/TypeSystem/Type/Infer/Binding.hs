module Compiler.Analysis.TypeSystem.Type.Infer.Binding where


import Control.Monad.Except
import Control.Monad.Reader

import Data.Functor.Identity

import qualified Data.Set as Set

import Data.List ((\\))


import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Binding

import Compiler.Analysis.TypeSystem.Utils.Infer
import Compiler.Analysis.TypeSystem.Utils.Class

import Compiler.Analysis.TypeSystem.InferenceEnv

import Compiler.Analysis.TypeSystem.Solver
import Compiler.Analysis.TypeSystem.Solver.Substitutable

import Compiler.Analysis.TypeSystem.Type.Infer.Match


{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
infer'expl :: Explicit -> Infer ([Predicate], [Constraint Type], [Constraint Kind])
infer'expl (Explicit scheme bg@Bind'Group{ name = name, alternatives = matches }) = do
  (qs :=> t) <- instantiate scheme
  (preds, cs't, cs'k) <- infer'matches matches t
  -- now solve it
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      let qs' = apply subst qs
          t'  = apply subst t
          -- so I need to apply the substitution to the typing context
          -- because that is how it's done in the THIH paper
      Infer'Env{ kind'env = k'env, type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
          gs = Set.toList (free'vars t') \\ fs
      let sc' = close'over (qs' :=> t')
          not'entail pred = do
            entailed <- entail c'env qs' pred
            return $ not entailed
      let ps' = runIdentity $ runExceptT $ filterM not'entail (apply subst preds)
      case ps' of
        Left err -> throwError err
        Right preds' -> do
          case runIdentity $ runExceptT $ split c'env fs gs preds' of
            Left err -> throwError err
            Right (deferred'preds, retained'preds) -> do
              if scheme /= sc' -- TODO: this line is going to break it as it is
                -- I first need to figure out how to do the comparison correctly, then implement Eq for Scheme manually
              then throwError Signature'Too'General
              else if not (null retained'preds)
              then throwError Context'Too'Weak
              else return (deferred'preds, cs't, cs'k)
