{-# LANGUAGE FlexibleContexts #-}

module Compiler.Counter where


import Control.Monad.State


newtype Counter = Counter{ counter :: Int }
  deriving (Show)


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: MonadState Counter m => m String
fresh = do
  Counter{ counter = counter } <- get
  put $ Counter{ counter = counter + 1 }
  return (letters !! counter)


-- TODO: I think it would be best to somehow figure these two out.
--        It seems like the `real'fresh` really does what is needed (fresh even when there are some already),
--        but right now, the fact that both exist, feels awkward.
real'fresh :: MonadState Counter m => [String] -> a -> m String
real'fresh vars var = do
  Counter{ counter = counter } <- get
  put $ Counter{ counter = counter + 1 }
  let name = letters !! counter
  if name `elem` vars
    then real'fresh vars var
    else return name
