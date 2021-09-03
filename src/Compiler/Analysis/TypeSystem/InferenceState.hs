module Compiler.Analysis.TypeSystem.InferenceState where


-- | Inference State
newtype Infer'State = Infer'State { count :: Int }

-- | initial Inference State
init'infer'state :: Infer'State
init'infer'state = Infer'State{ count = 0 }