module Compiler.TypeSystem.InferenceState where


import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Overloaded ( Overloaded ) 

import Compiler.Counter ( Counter, State(..) )

import Compiler.TypeSystem.Constraint ( Constraint )

-- type Infer'State = Counter


data Infer'State a = Infer'State  { counter :: Counter
                                  , constraints :: [Constraint a]
                                  , overloaded :: [(Name, Overloaded)]
                                  , instances :: [((Name, Type), (Name, [Predicate], Predicate))]
                                  , holes :: [(Name, Type)] }
      deriving (Show)


instance State (Infer'State a) where
  get'counter Infer'State{ counter = counter }
    = counter
  update'counter counter infer'state
    = infer'state{ counter = counter }

