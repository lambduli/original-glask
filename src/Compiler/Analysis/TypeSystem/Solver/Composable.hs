{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Analysis.TypeSystem.Solver.Composable where


import Data.List
import qualified Data.Map.Strict as Map
import Control.Monad.Except


import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Solve
import Compiler.Analysis.TypeSystem.Solver.Inject



class Composable k v where
  compose :: (Ord k, Substitutable k v v) => Subst k v -> Subst k v -> Subst k v
  (Sub sub'l) `compose` (Sub sub'r)
      = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l

  -- I know this code looks terrible
  -- but there is a good reason to have it look like that (lot's of constraints and scoped types)
  -- TODO: explain in greater detail how it works both from evaluation and type level viewpoint
  merge :: (Ord k, Substitutable k v v, Inject k v, Eq v) => Subst k v -> Subst k v -> Solve (Subst k v)
  s1@(Sub sub'l) `merge` s2@(Sub sub'r)
    = if agree
      then return $ Sub $ sub'l `Map.union` sub'r
      else throwError $ Unexpected "merge fails"
        where
          agree = all equivalent (Map.keys sub'l `intersect` Map.keys sub'r)
          equivalent :: (Inject k v, Substitutable k v v) => k -> Bool
          equivalent var = apply s1 injected == apply s2 injected
            where
              injected :: v
              injected = inject var



-- TODO: for this line to work, I will need to define two instances for Substituable

-- the line I am talking about:
-- all (\ var -> apply s1 var == apply s2 var) (Map.keys sub'l `intersect` Map.keys sub'r)


-- one for T'V T'V Type
-- and the second for String String Kind (I think)
-- problem is - that does not make sense!
-- you can't apply substitution (replacing T'V with a Type) to a T'V
-- that is simply nonsense - 

-- I can do two things
-- I can define a type class with a method, - define two instances like this:
-- one will take T'V and produce Type through T'Var
-- the other takes a String a produces Kind through K'Var
-- that would work, BUT - it is really not obvious why am I doing that
-- and the line I am trying to fix - is going to be ubscured by the opaqueness of this operation


-- second option is to invert the problem
-- instead of figuring out how to transform the T'V or String to Type or Kind
-- and be able to use the apply
-- I could define operation which would take two substitutions and a variable
-- it would then decide whether these two are equivalent in terms of substitution of this one variable
-- it would need to perform the `apply` in any case (anything else would be unreasonable)
-- but then again, the problem is, I can't define Substituable for T'V T'V Type and make the apply return a T'V
-- which I would need to, because the instance would work like this:
-- first inject the T'V into a Type through T'Var constructor
-- then call `apply` with the substitution
-- this results in the result of a type Type
-- but since the class definition forces me to return T'V
-- I would need to project the Type back to T'V
-- that is a partial function though -> so I won't be able to compare two results from two substs
-- 

-- possible solution would be - implement a type class (like Composable) for substitutions
-- and implement the merge and compose as default methods while the first mentioned would use
-- a method inject - which later would be defined for T'V as a T'Var and for String as a K'Var
-- what bugs me though - is that the inject doesn't really have anything to do with the Composability
-- of the substitutions
-- so maybe make another class Injectable or something
-- there define the operation for T'V and String
-- then constraint the merge method with this

-- TODO: I am going to remove these too (same as the Composable class declaration)
-- I just want to keep them for a little while

instance Composable T'V Type
-- instance Composable TVar Type where
--   -- compose :: (Substitutable k a a, Ord k) => Subst k a -> Subst k a -> Subst k a
--   (Sub sub'l) `compose` (Sub sub'r)
--     = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l

--   -- {- symmetric merge function from the paper Typing Haskell in Haskell -}
--   -- merge :: (Substitutable TVar a a, Monad m) => Subst TVar a -> Subst TVar a -> m (Subst TVar a)
--   s1@(Sub sub'l) `merge` s2@(Sub sub'r)
--     = if agree then return $ Sub $ sub'l `Map.union` sub'r else throwError $ Unexpected "merge fails"
--       where
--         agree = all (\ var -> apply s1 (TyVar var) == apply s2 (TyVar var)) (Map.keys sub'l `intersect` Map.keys sub'r)

instance Composable String Kind
-- instance Composable String Kind where
--   (Sub sub'l) `compose` (Sub sub'r)
--     = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l

--   s1@(Sub sub'l) `merge` s2@(Sub sub'r)
--     = if agree then return (Sub $ sub'l `Map.union` sub'r) else throwError $ Unexpected "merge fails"
--       where
--         agree = all (\ var -> apply s1 (KVar var) == apply s2 (KVar var)) (Map.keys sub'l `intersect` Map.keys sub'r)



-- class Composable k v where
--   compose :: (Ord k, Substitutable k v v) => Subst k v -> Subst k v -> Subst k v
--   -- compose :: (Ord k, Substitutable k v v) => Subst k v -> Subst k v -> Subst k v
--   (Sub sub'l) `compose` (Sub sub'r)
--       = Sub $ Map.map (apply (Sub sub'l)) sub'r `Map.union` sub'l

--   merge :: (Ord k {-, Substitutable k k v-} , Inject k v) => Subst k v -> Subst k v -> Solve (Subst k v)
--   -- merge :: Subst k v -> Subst k v -> Solve (Subst k v)
--   s1@(Sub sub'l) `merge` s2@(Sub sub'r)
--     = if agree
--       then return $ Sub $ sub'l `Map.union` sub'r
--       else throwError $ Unexpected "merge fails"
--         where
--           agree = all (\ var -> apply s1 (inject var) == apply s2 (inject var)) (Map.keys sub'l `intersect` Map.keys sub'r)
