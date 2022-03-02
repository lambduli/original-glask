module Compiler.TypeSystem.Kind.Infer.Type where

import qualified Data.Map.Strict as Map
import Control.Monad.State ( foldM )
import Control.Monad.Extra ( concatMapM )
import Control.Monad.Except ( MonadError(throwError) )
import Control.Monad.Trans.Reader ( asks )


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(..) )
import Compiler.Syntax.Predicate ( Predicate (Is'In) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(T'C), T'V(T'V), Type(..) )

import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(constraint'env, kind'env) )
import Compiler.TypeSystem.Error ( Error(Unexpected) )


infer'type :: Type -> Infer (Kind, [Constraint Kind])
infer'type (T'Var (T'V name _)) = do
  k'env <- asks kind'env
  
  case k'env Map.!? name of
    Nothing -> throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have found a type variable '" ++ name ++ "' which is not registered in the Kind Context."
    Just kind -> return (kind, [])

infer'type (T'Con (T'C name _)) = do
  k'env <- asks kind'env
  
  case k'env Map.!? name of
    Nothing -> throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have found a type constant '" ++ name ++ "' which is not registered in the Kind Context."
    Just kind -> return (kind, [])
  

infer'type (T'Tuple types) = do
  (kinds, cs'k) <- infer'types types
  return (K'Star, cs'k)

infer'type (T'App left't right't) = do
  (k'l, cs'l) <- infer'type left't
  (k'r, cs'r) <- infer'type right't
  fresh'name <- fresh
  let var = K'Var fresh'name
  let constraint = k'l `Unify` (k'r `K'Arr` var)

  return (var, constraint : cs'l ++ cs'r)

infer'type (T'Forall tvs (preds :=> type')) = do
  let assumptions = map (\ (T'V n k) -> (n, k)) tvs

  cs'k'ps <- merge'into'k'env assumptions (infer'context preds)
  (k't, cs'k't) <- merge'into'k'env assumptions (infer'type type')

  return (k't, cs'k'ps ++ cs'k't)


infer'types :: [Type] -> Infer ([Kind], [Constraint Kind])
infer'types types = do
  foldM infer'type' ([], []) types
    where
      infer'type' (ks, cs) t = do
        (k, cs') <- infer'type t
        return (k : ks, cs' ++ cs)


-- infer'preds :: [Predicate] -> Infer [Constraint Kind]
-- infer'preds = undefined
-- TODO:  v environmentu musim mit vytvoreny Kind Env pro Type Classy
--        kazda type class ma prirazeny jediny Kind - Kind jejiho parametru
--        ten vytahnu a vytvorim binding na Kind Typu (typove promenne) uvnitr toho predikatu
--


{- THIS IS NEW -}
infer'context :: [Predicate] -> Infer [Constraint Kind ]
infer'context context = do concatMapM infer'predicate context


infer'predicate :: Predicate -> Infer [Constraint Kind]
infer'predicate (Is'In cl'name type') = do
  (k, k'cs) <- infer'type type'

  constraint'env <- asks constraint'env

  param'kind <- case constraint'env Map.!? cl'name of
    Nothing -> throwError $ Unexpected $ "Unexpected: While doing a kind inference I have found a Predicate '" ++ cl'name ++ "' which is not registered in the Constraint Environment."
    Just kind -> return kind

  return (k `Unify` param'kind : k'cs)
{- END OF NEW -}