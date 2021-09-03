module Compiler.Analysis.TypeSystem.Ambiguity where


import Data.Set (difference, fromList, toList)
import Data.List (filter)
import Data.Foldable.Extra (allM)

import Compiler.Syntax

import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.Solver.Substitutable
-- import Compiler.Analysis.TypeSystem.ClassTMP
import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Solver
import Compiler.Analysis.TypeSystem.Solver.Solve
import Compiler.Analysis.TypeSystem.Utils.Class


type Ambiguity = (T'V, [Predicate])


{- following implementation is really awkward, especially the right part of the list comprehension -}
-- TODO: ^^^ so try to refactor it
ambiguities :: Class'Env -> [T'V] -> [Predicate] -> [Ambiguity]
ambiguities cl'env vars preds = [(v, filter (elem v . free'vars) preds) | v <- toList $ free'vars preds `difference` fromList vars]


num'classes :: [Name]
num'classes
  = ["Eq", "Integral", "Floating", "Fractional", "Real", "RealFloat", "RealFrac"]


std'classes :: [Name]
std'classes
  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor", "Monad", "MonadPlus"]
    ++ num'classes


{- TODO: refactor a little -}
candidates :: Class'Env -> Ambiguity -> Solve [Type]
candidates cl'env (v, qs) = do
  let is = [i | Is'In i t <- qs]
  let ts = [t | Is'In i t <- qs]
  let ts' = [t' | all (T'Var v ==) ts,
                  any (`elem` num'classes) is,
                  all (`elem` std'classes) is,
                  t' <- defaults cl'env]
  ok <- allM (entail cl'env []) [Is'In i t' | i <- is, t' <- ts']
  return $ if ok then ts' else []