{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Analysis.TypeSystem.Solver.Unify where


import Control.Monad.Except

import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Solve
import Compiler.Analysis.TypeSystem.Solver.Bind
import Compiler.Analysis.TypeSystem.Solver.Composable


{-  Typing Haskell in Haskell commentary:
    M. P. Jones calls it `mgu` as in Most General Unifier.
    He also doesn't define it for a list of values -> no unify'many
    that would probably make things little bit more elegant and clean.
    TODO: Definitely consider refactoring!
-}
{-  Unify class represents ability tu unify and produce Substitution
    `k` represents the "key" of the Substitution [usually String or TVar]
    `a` represents the type of values which will be unified
    `x` represents the "value" of the Substitution which will be produced by the unification
        usually it will be the same as `a` (Kind or Type), but sometimes it will be different
        as is the case of the Predicate - unifying two predicates produces a Substitution of TVar Type
-}
class Unify a k x where
  unify :: a -> a -> Solve (Subst k x)
  match :: a -> a -> Solve (Subst k x) -- only one-way-going unification


-- TODO: try to find a way to get rid of (the need for) this class and method
class Unify'Many k a b where
  unify'many :: a b -> a b -> Solve (Subst k b)


{-               a   k   x       -}
instance Unify Type T'V Type where
  unify t1 t2 | t1 == t2
    = return empty'subst

  unify (T'Var var) t
    = var `bind` t

  unify t (T'Var var)
    = var `bind` t
  -- TODO: use the k's to make sure we are unifying only Type Variable of specific Kind with the Type of the same Kind

  unify (T'App t1 t2) (T'App t3 t4)
    = [t1, t2] `unify'many` [t3, t4]

  unify l@(T'Con t'con'l) r@(T'Con t'con'r)
    | t'con'l == t'con'r = return empty'subst
    | otherwise = throwError $ Type'Unif'Mismatch l r
    -- TODO: use the kinds to make sure we are unifying only Type Constants of the same Kind

  unify (T'Tuple ts'left) (T'Tuple ts'right)
    = if length ts'left /= length ts'right
      then throwError $ Type'Shape'Mismatch (T'Tuple ts'left) (T'Tuple ts'right)
      else ts'left `unify'many` ts'right

  unify t1 t2
    = throwError $ Type'Shape'Mismatch t1 t2

  match t1 t2 | t1 == t2
    = return empty'subst

  match (T'Var var) t | kind var == kind t
    = var `bind` t

  match (T'App l r) (T'App l' r') = do
    sub'l <- l `match` l'
    sub'r <- r `match` r'
    sub'l `merge` sub'r

  match (T'Con con'l) (T'Con con'r) | con'l == con'r
    = return empty'subst

  match t1 t2
    = throwError $ Type'Shape'Mismatch t1 t2


{-              a    k      x       -}
instance Unify Kind String Kind where
  unify t1 t2 | t1 == t2
    = return empty'subst

  unify (K'Var v) k
    = v `bind` k
  
  unify k (K'Var v)
    = v `bind` k

  unify (K'Arr k1 k2) (K'Arr k3 k4)
    = [k1, k2] `unify'many` [k3, k4]

  unify K'Star K'Star
    = return empty'subst

  unify k1 k2
    = throwError $ Kind'Shape'Mismatch k1 k2

  match t1 t2 | t1 == t2
    = return empty'subst

  match (K'Var v) k
    = v `bind` k

  match (K'Arr k1 k2) (K'Arr k3 k4) = do
    sub'l <- k1 `match ` k3
    sub'r <- k2 `match ` k4
    sub'l `merge` sub'r
  
  match K'Star K'Star = return empty'subst

  match k1 k2
    = throwError $ Kind'Shape'Mismatch k1 k2


instance Unify'Many T'V [] Type where
  unify'many [] []
    = return empty'subst

  unify'many (t'l : ts'l) (t'r : ts'r) = do
    su1 <- t'l `unify` t'r
    su2 <- apply su1 ts'l `unify'many` apply su1 ts'r
    return (su2 `compose` su1)

  unify'many t'l t'r
    = throwError $ Type'Unif'Count'Mismatch t'l t'r


instance Unify'Many String [] Kind where
  unify'many [] []
    = return empty'subst
  
  unify'many (t'l : ts'l) (t'r : ts'r) = do
    su1 <- t'l `unify` t'r
    su2 <- apply su1 ts'l `unify'many` apply su1 ts'r
    return (su2 `compose` su1)
  
  unify'many t'l t'r
    = throwError $ Kind'Unif'Count'Mismatch t'l t'r


instance Unify Predicate T'V Type where
  {- NOTE: The duplicity in the implementation of `lift` is really bothersome. -}
  unify = lift unify
    where
      lift :: (Type -> Type -> Solve (Subst T'V Type)) -> Predicate -> Predicate -> Solve (Subst T'V Type)
      lift fn (Is'In name'l type'l) (Is'In name'r type'r)
        | name'l == name'r  = fn type'l type'r
        | otherwise         = throwError $ Unexpected $ "Unification Error: Type Classes `" ++ name'l ++ "` and `" ++ name'r ++ "` differ and can not be unified."

  match = lift match
    where
      lift :: (Type -> Type -> Solve (Subst T'V Type)) -> Predicate -> Predicate -> Solve (Subst T'V Type)
      lift fn (Is'In name'l type'l) (Is'In name'r type'r)
        | name'l == name'r  = fn type'l type'r
        | otherwise         = throwError $ Unexpected $ "Unification Error: Type Classes `" ++ name'l ++ "` and `" ++ name'r ++ "` differ and can not be unified."