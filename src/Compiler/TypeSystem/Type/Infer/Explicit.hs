module Compiler.TypeSystem.Type.Infer.Explicit where


import Control.Monad.Except
import Control.Monad.Reader

import Data.Functor.Identity

import qualified Data.Set as Set

import Data.List


import Compiler.Syntax

import Compiler.TypeSystem.Error
import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Binding
import Compiler.TypeSystem.Utils.Infer
import Compiler.TypeSystem.Utils.Class
import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.Solver
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.Type.Infer.Match
import Compiler.TypeSystem.Kind.Infer.Type (infer'type)



{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
{- Returning a [Constraint Type] might not be strictly necessary -}
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
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
          gs = Set.toList (free'vars t') \\ fs
      let sc' = close'over (qs' :=> t')
          not'entail pred = do
            -- not <$> entail c'env qs' pred -- this should be the same thing, but more succinctly written
            entailed <- entail c'env qs' pred
            return $ not entailed
      let ps' = runIdentity $ runExceptT $ filterM not'entail (apply subst preds)
      case ps' of
        Left err -> throwError err
        Right preds' -> do
          case runIdentity $ runExceptT $ split c'env fs gs preds' of
            Left err -> throwError err
            Right (deferred'preds, retained'preds) -> do
              (k, cs'k') <- infer'type t

              if scheme /= sc'
              {- TODO:  If I want to know exactly what user-denoted type variable in the `scheme` does correspond to some non-variable type, I can use `match` to create a one-way substitution. -}
              then throwError Signature'Too'General
              else  if not (null retained'preds)
                    then throwError Context'Too'Weak
              else return (deferred'preds, cs't, (k `Unify` K'Star) : cs'k ++ cs'k')
              -- TODO:  FIX - here the `kind` function is not used safely
              --        the problem is that kind function expects for Type Applications - that the Kind of the Left part will be Kind Arrow
              --        but that's never going the happen for types given from the user (annotations)
              --        those types contain fresh Kind Variables and not Kind Arrows in general
              --        of course - function types will work - since the (->) is builtin type constructor with the correct Kind assigned in the system
              --        but annotations like foo :: m a
              --        would break the system because (m :: ?A) and (a :: ?B)
              --        and the left part definitely does not give Kind Arrow
              --        therefore I am all in favour of the proposition to change the `kind` method to also produce [Constraint Kind]
              --        instead of expecting the left kind to be arrow, it can also work with Kind Variable on the left and simply produce additional constraints to make sure
              --        everything stays sound
              --
              -- NOTE: Fixed - or it seems.
