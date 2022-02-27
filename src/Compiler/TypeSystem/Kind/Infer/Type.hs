module Compiler.TypeSystem.Kind.Infer.Type where


import Compiler.Counter

import Control.Monad.State ( foldM )


import Compiler.Syntax.Kind ( Kind(..) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(T'C), T'V(T'V), Type(..) )

import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env )


infer'type :: Type -> Infer (Kind, [Constraint Kind])
infer'type (T'Var (T'V name kind))
  = return (kind, [])

infer'type (T'Con (T'C name kind))
  = return (kind, [])

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

  cs'k'ps <- merge'into'k'env assumptions (infer'preds preds)
  (k't, cs'k't) <- merge'into'k'env assumptions (infer'type type')

  return (k't, cs'k'ps ++ cs'k't)


infer'types :: [Type] -> Infer ([Kind], [Constraint Kind])
infer'types types = do
  foldM infer'type' ([], []) types
    where
      infer'type' (ks, cs) t = do
        (k, cs') <- infer'type t
        return (k : ks, cs' ++ cs)


infer'preds :: [Predicate] -> Infer [Constraint Kind]
infer'preds = undefined
-- TODO:  v environmentu musim mit vytvoreny Kind Env pro Type Classy
--        kazda type class ma prirazeny jediny Kind - Kind jejiho parametru
--        ten vytahnu a vytvorim binding na Kind Typu (typove promenne) uvnitr toho predikatu
--