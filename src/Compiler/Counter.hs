{-# LANGUAGE FlexibleContexts #-}

module Compiler.Counter where


import Control.Monad.State ( MonadState(put, get), replicateM )


newtype Counter = Counter{ counter :: Int }
  deriving (Show)


class State a where
  get'counter :: a -> Counter
  update'counter :: Counter -> a -> a

instance State Counter where
  get'counter = id
  update'counter counter _ = counter


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: (MonadState c m, State c) => m String
fresh = do
  state <- get
  let Counter{ counter  = counter } = get'counter state
      new'counter       = Counter{ counter = counter + 1 }
      new'state         = update'counter new'counter state
  put new'state
  return (letters !! counter)


-- TODO: I think it would be best to somehow figure these two out.
--        It seems like the `real'fresh` really does what is needed (fresh even when there are some already),
--        but right now, the fact that both exist, feels awkward.
real'fresh :: (MonadState c m, State c) => [String] -> a -> m String
real'fresh vars var = do
  state <- get
  let Counter{ counter = counter }  = get'counter state
      new'counter                   = Counter{ counter = counter + 1 }
      new'state                     = update'counter new'counter state
  put new'state
  let name = letters !! counter
  if name `elem` vars
    then real'fresh vars var
    else return name
