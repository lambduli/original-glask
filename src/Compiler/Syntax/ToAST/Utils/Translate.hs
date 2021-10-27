module Compiler.Syntax.ToAST.Utils.Translate where

import Control.Monad.State

import Compiler.Syntax.ToAST.Translate
import Compiler.Syntax.ToAST.TranslateState


-- | NOTE: Code in here is the same as in the Utils for the Type Inference
-- | TODO: Solve the redundancy and fix this

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Translate String
fresh = do
  Translate'State { count = counter } <- get
  put $ Translate'State { count = counter + 1 }
  return (letters !! counter)
