{-# LANGUAGE MultiParamTypeClasses  #-}

module Compiler.Analysis.Extract where


class Extract a b where
  extract :: a -> b
