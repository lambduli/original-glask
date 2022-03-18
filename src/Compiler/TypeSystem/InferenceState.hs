module Compiler.TypeSystem.InferenceState where


import Compiler.Counter ( Counter, State(..) )

import Compiler.TypeSystem.Constraint ( Constraint )

-- type Infer'State = Counter


data Infer'State a = Infer'State { counter :: Counter, constraints :: [Constraint a] }


instance State (Infer'State a) where
  get'counter Infer'State{ counter = counter }
    = counter
  update'counter counter infer'state
    = infer'state{ counter = counter }

