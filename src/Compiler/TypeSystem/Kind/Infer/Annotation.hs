{-# LANGUAGE TupleSections #-}

module Compiler.TypeSystem.Kind.Infer.Annotation where


import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.State ( MonadState(get, put), evalStateT, StateT )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type (T'Forall), T'V'(T'V') )
import Compiler.Syntax.Kind ( Kind (K'Star) )

import Compiler.TypeSystem.Infer ( Infer, Kind'Check, Type'Check, run'infer, get'constraints )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'type )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Error ( Error )

import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst(Sub) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term (free'vars) )
import Compiler.Counter ( State(get'counter, update'counter) )
import Compiler.TypeSystem.InferenceState ( Infer'State(..) )


{-  This module is supposed to be used from `infer'expl` and `check'method.
    It implementes function `kind'infer'sigma` which takes a Sigma Type and either fails
    or produces that same sigma but fully Kind specified / inferred.
    So that when the result get instantiated all type variables introduced into the context
    have known Kinds.
    It does use Kind Defaulting to * for all unsolved Kind Variables.
-}

kind'specify :: Sigma'Type -> Type'Check Sigma'Type
kind'specify sigma = do
  infer'env <- ask
  infer'state <- get
  let counter       = get'counter infer'state
      infer'state'  = Infer'State{ counter = counter, constraints = [] }

  case run'infer infer'env (kind'infer'sigma sigma) infer'state' of
    Left err ->
      throwError err
    Right (sigma, state) -> do
      let counter = get'counter state
      let new'state = update'counter counter infer'state
      put new'state

      return sigma


kind'infer'sigma :: Sigma'Type -> Kind'Check Sigma'Type
kind'infer'sigma sigma = do
  kind <- infer'type sigma

  k'cs <- get'constraints

  case run'solve ((kind `Unify` K'Star): k'cs) :: Either Error (Subst Name Kind) of
    Left error -> throwError error
    Right subst -> do
      let sigma'@(T'Forall tvs _) = apply subst sigma

      let defaulting'mapping  = make'default $ map (\ (T'V' _ k) -> k) tvs
          defaulting'subst    = Sub $ Map.fromList defaulting'mapping

      let sigma'' = apply defaulting'subst sigma'

      return sigma''


make'default :: [Kind] -> [(Name, Kind)]
make'default kinds = map (, K'Star) all'kind'variables
  where
    all'kind'variables :: [Name]
    all'kind'variables = Set.toList $ free'vars kinds
