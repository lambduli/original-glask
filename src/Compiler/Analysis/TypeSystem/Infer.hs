module Compiler.Analysis.TypeSystem.Infer where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.InferenceState


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
