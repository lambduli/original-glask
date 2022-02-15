module Compiler.TypeSystem.Infer (Infer, run'infer) where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.TypeSystem.Error

import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.InferenceState


-- | Inference Monad
type Infer a
  = ReaderT
      Infer'Env
      (StateT
        Infer'State
        (Except
          Error))
      a


run'infer :: Infer'Env -> Infer a -> Infer'State -> Either Error (a, Infer'State)
run'infer env m i'state = runExcept $ evalStateT (runReaderT (run'infer' m) env) i'state


run'infer' :: Infer a -> Infer (a, Infer'State)
run'infer' m = do
  a <- m
  inf'state <- get
  return (a, inf'state)
