module Compiler.Syntax.ToAST.Utils.Translate where

import Control.Monad.State


import Compiler.Counter ( Counter(Counter, counter) )

import Compiler.Syntax.ToAST.Translate


-- | NOTE: Code in here is the same as in the Utils for the Type Inference
-- | TODO: Solve the redundancy and fix this

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Translate String
fresh = do
  Counter{ counter = counter } <- get
  put $ Counter{ counter = counter + 1 }
  return (letters !! counter)
