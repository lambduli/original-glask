module Compiler.TypeSystem.InferenceState where


import Compiler.Counter

type Infer'State = Counter


-- | Inference State
-- newtype Infer'State = Infer'State { count :: Int }

-- | initial Inference State
-- init'infer'state :: Infer'State
-- init'infer'state = Infer'State{ count = 0 }
