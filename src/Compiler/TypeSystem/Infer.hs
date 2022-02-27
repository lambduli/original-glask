module Compiler.TypeSystem.Infer (Infer, run'infer) where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( MonadState(get), evalStateT, StateT )
import Control.Monad.Except ( Except, runExcept )


import Compiler.TypeSystem.Error ( Error )

import Compiler.TypeSystem.InferenceEnv ( Infer'Env )
import Compiler.TypeSystem.InferenceState ( Infer'State )


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
