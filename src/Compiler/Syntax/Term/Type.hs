{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Type where


import qualified Data.Set as Set

import Compiler.Syntax.Term.Identifier

import Compiler.TypeSystem.Solver.Substitutable


data Term'Type
  = Term'T'Id Term'Id
  | Term'T'Tuple [Term'Type]
  | Term'T'List Term'Type
  | Term'T'Arrow [Term'Type]
  | Term'T'App [Term'Type]
  deriving (Eq)


-- | NOTE: This instance declaration is supposed to be used when translating `to'ast`.
-- |        When I need to know all free variables in the (qualified) Term'Type.
instance Term Term'Id Term'Type where
  free'vars (Term'T'Id var@(Term'Id'Var _))
    = Set.singleton var

  free'vars (Term'T'Id (Term'Id'Const _))
    = Set.empty

  free'vars (Term'T'Tuple term'types)
    = foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty term'types

  free'vars (Term'T'List term'type)
    = free'vars term'type

  free'vars (Term'T'Arrow term'types)
    = foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty term'types

  free'vars (Term'T'App term'types)
    = foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty term'types
