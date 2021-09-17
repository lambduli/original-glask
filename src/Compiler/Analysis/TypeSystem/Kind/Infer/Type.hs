module Compiler.Analysis.TypeSystem.Kind.Infer.Type where


import Data.Tuple.Extra
import Control.Monad.State


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Infer

import Compiler.Analysis.TypeSystem.Solver.Unify

import Compiler.Analysis.TypeSystem.Utils.Infer


infer'type :: Type -> Infer (Kind, [Constraint Kind])
infer'type (T'Var (T'V name kind))
  = return (kind, [])

infer'type (T'Con (T'C name kind))
  = return (kind, [])

infer'type (T'Tuple types) = do
  (kinds, cs'k) <- infer'types types
  undefined

infer'type (T'App left't right't) = do
  (k'l, cs'l) <- infer'type left't
  (k'r, cs'r) <- infer'type right't
  fresh'name <- fresh
  let var = K'Var fresh'name
  let constraint = k'l `Unify` (k'r `K'Arr` var)

  return (var, constraint : cs'l ++ cs'r)


infer'types :: [Type] -> Infer ([Kind], [Constraint Kind])
infer'types types = do
  foldM infer'type' ([], []) types
    where
      infer'type' (ks, cs) t = do
        (k, cs') <- infer'type t
        return (k : ks, cs' ++ cs)
