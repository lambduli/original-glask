module Compiler.TypeSystem.Infer where


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


run'infer :: Infer'Env -> Infer a -> Either Error a
run'infer env m = runExcept $ evalStateT (runReaderT m env) init'infer'state
