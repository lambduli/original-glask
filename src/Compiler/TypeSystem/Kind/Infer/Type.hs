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
import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(T'C), T'V'(T'V'), Type(..), M'V(..) )

import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Infer ( Infer, Kind'Check, add'constraints )
import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(constraint'env, kind'env) )
import Compiler.TypeSystem.Error ( Error(Unexpected) )


infer'type :: Type -> Kind'Check Kind
infer'type (T'Var' (T'V' name _)) = do
  k'env <- asks kind'env
  
  case k'env Map.!? name of
    Nothing -> throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have found a type variable '" ++ name ++ "' which is not registered in the Kind Context."
    Just kind -> return kind

-- BIG TODO: nejsem si uplne jistej, ale musim M'V taky implementovat
infer'type (T'Meta (Tau name _)) = do
  k'env <- asks kind'env

  case k'env Map.!? name of
    Nothing -> throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have found a meta type variable '" ++ name ++ "' which is not registered in the Kind Context."
    Just kind -> return kind

infer'type (T'Meta (Sigma name _)) = do
  k'env <- asks kind'env

  case k'env Map.!? name of
    Nothing -> throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have found a meta type variable '" ++ name ++ "' which is not registered in the Kind Context."
    Just kind -> return kind

infer'type (T'Con (T'C name _)) = do
  k'env <- asks kind'env
  
  case k'env Map.!? name of
    Nothing -> throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have found a type constant '" ++ name ++ "' which is not registered in the Kind Context."
    Just kind -> return kind
  

infer'type (T'Tuple types) = do
  kinds <- infer'types types
  return K'Star

infer'type (T'App left't right't) = do
  k'l <- infer'type left't
  k'r <- infer'type right't
  fresh'name <- fresh
  let var = K'Var fresh'name

  add'constraints [k'l `Unify` (k'r `K'Arr` var)]
  return var

infer'type (T'Forall tvs (preds :=> type')) = do
  let assumptions = map (\ (T'V' n k) -> (n, k)) tvs

  cs'k'ps <- merge'into'k'env assumptions (infer'context preds)
  merge'into'k'env assumptions (infer'type type')


infer'types :: [Type] -> Kind'Check [Kind]
infer'types types = do
  foldM infer'type' [] types
    where
      infer'type' ks t = do
        k <- infer'type t
        return (k : ks)


-- infer'preds :: [Predicate] -> Infer [Constraint Kind]
-- infer'preds = undefined
-- TODO:  v environmentu musim mit vytvoreny Kind Env pro Type Classy
--        kazda type class ma prirazeny jediny Kind - Kind jejiho parametru
--        ten vytahnu a vytvorim binding na Kind Typu (typove promenne) uvnitr toho predikatu
--


{- THIS IS NEW -}
infer'context :: [Predicate] -> Kind'Check ()
infer'context context = do mapM_ infer'predicate context


infer'predicate :: Predicate -> Kind'Check ()
infer'predicate (Is'In cl'name type') = do
  k <- infer'type type'

  constraint'env <- asks constraint'env

  param'kind <- case constraint'env Map.!? cl'name of
    Nothing -> throwError $ Unexpected $ "Unexpected: While doing a kind inference I have found a Predicate '" ++ cl'name ++ "' which is not registered in the Constraint Environment."
    Just kind -> return kind

  add'constraints [k `Unify` param'kind]
  return ()
{- END OF NEW -}