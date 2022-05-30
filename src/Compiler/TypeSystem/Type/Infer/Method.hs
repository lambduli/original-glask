module Compiler.TypeSystem.Type.Infer.Method where


import Control.Monad.Except ( filterM, runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( filterM, MonadReader(ask) )
import Control.Monad.Extra ( filterM )
import Data.Functor.Identity ( Identity(runIdentity) )
import qualified Data.Set as Set
import Data.List ( (\\) )


import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name, alternatives) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'V', Type (T'Forall), M'V )

import Compiler.TypeSystem.Error ( Error(..) )
import Compiler.TypeSystem.Infer ( Infer, Type'Check, get'constraints )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Binding ( Method(..) )
import Compiler.TypeSystem.Utils.Infer ( instantiate, split', skolemise, close'over', phs'matches )
import Compiler.TypeSystem.Utils.Class ( entail )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, class'env) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Infer.Match ( check'matches )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'infer'sigma, kind'specify )
import Compiler.TypeSystem.Expected ( Expected(Check) )


{-  Description:

    The `infer'method` function infers and checks a type for the method binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
{- Returning a [Constraint Type] might not be strictly necessary -}
infer'method :: Method -> Type'Check (Method, [Predicate])
infer'method (Method scheme bg@Bind'Group{ name = name, alternatives = matches } cl'name dict'name) = do
  -- scheme' <- kind'infer'sigma scheme -- this is no longer necessary - it's been done before-hand
  
  -- (qs :=> t) <- instantiate scheme
  (skolems, qs, t) <- skolemise scheme
  -- STEJNEJ DUVOD JAKO U EXPLICIT - HLEDEJ KOMENTAR A VYSVETLENI TAM

  (matches', preds) <- check'matches matches t
  -- now solve it
  cs't <- get'constraints
  case run'solve cs't :: Either Error (Subst M'V Type) of
    Left err ->
      throwError err

    Right subst -> do
      let qs' = apply subst qs
          t'  = apply subst t
          -- so I need to apply the substitution to the typing context
          -- because that is how it's done in the THIH paper

      Infer'Env{ type'env = t'env, class'env = c'env } <- ask

      let fs = Set.toList $ free'vars $ apply subst t'env
          gs = Set.toList (free'vars t') \\ fs
      let sc' = close'over' (qs' :=> t')
          not'entail pred = do
            -- not <$> entail c'env qs' pred -- this should be the same thing, but more succinctly written
            entailed <- entail c'env qs' pred
            return $ not entailed

      let ps' = runIdentity $ runExceptT $ filterM not'entail (apply subst preds)
      
      case ps' of
        Left err ->
          throwError err

        Right preds' -> do
          case runIdentity $ runExceptT $ split' c'env fs skolems gs preds' of
            Left err ->
              throwError err

            Right (deferred'preds, retained'preds) -> do
              {- TODO:  If I want to know exactly what user-denoted type
              variable in the `scheme'` does correspond to some non-variable
              type, I can use `match` to create a one-way substitution. -}
              if not (null retained'preds)
                then throwError Context'Too'Weak
                else do
                  let matches'' = map (phs'matches subst) matches'
                  let scheme'   = T'Forall skolems (qs :=> t)
                  let method'   = Method scheme' bg{ alternatives = matches'' } cl'name dict'name
                  return (method', deferred'preds)
