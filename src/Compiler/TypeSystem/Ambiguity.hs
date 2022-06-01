{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Ambiguity where


import Data.Set (difference, fromList, toList)
import Data.List (filter)
import Data.Foldable.Extra (allM)
import Control.Monad (filterM)
import Control.Monad.Except ( MonadError )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate(..) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type(T'Meta), M'V )

import Compiler.TypeSystem.ClassEnv ( Class'Env(defaults) )
import Compiler.TypeSystem.Solver.Substitutable ( Term(free'vars) )
-- import Compiler.TypeSystem.Solver.Solve ( Solve )
import Compiler.TypeSystem.Utils.Class ( entail )
import Compiler.TypeSystem.Error (Error)


type Ambiguity = (M'V, [Predicate])


{- following implementation is really awkward, especially the right part of the list comprehension -}
-- TODO: ^^^ so try to refactor it
ambiguities :: Class'Env -> [M'V] -> [Predicate] -> [Ambiguity]
ambiguities cl'env vars preds = [(v, filter (elem v . free'vars) preds) | v <- toList $ free'vars preds `difference` fromList vars]


num'classes :: [Name]
num'classes
  = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]


std'classes :: [Name]
std'classes
  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"]
    ++ num'classes


{- TODO: refactor a little -}
{-  TODO: I have seriously messed up this function.
          First of all - the original was wrong.
          Now I have hopefully fixed it. But I need to rewrite it and remove the traces.
          While doing so - really carefully check that it's correct.
-}
candidates :: MonadError Error m => Class'Env -> Ambiguity -> m [Type]
candidates cl'env (v, qs) = do
  let is = [i | Is'In i t <- qs] -- all Class names from the constraints
  let ts = [t | Is'In i t <- qs] -- all types/parameters of the constraints
  let ts' = [t' | all (T'Meta v ==) ts, -- all the types must be just a `Type Variable v`
                  any (`elem` num'classes) is, -- at least one of the classes must be standard numerical class
                  all (`elem` std'classes) is, -- all of them are standard classes
                  t' <- defaults cl'env]
  let
    entails :: MonadError Error m => Type -> m Bool
    entails t =
      let
        constraints :: [Predicate] 
        constraints = [Is'In i t | i <- is]
      in return $ all (entail cl'env []) constraints
  filterM entails ts' -- [Is'In i t' | i <- is, t' <- aa]
  -- return $ if ok then ts' else []
