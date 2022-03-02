{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Analysis.Semantic.Dependency.Depends where


import qualified Data.Map.Strict as Map

-- Depends type class represents a relation between two types
-- `a` is a type of the value which depends on something (like Expression or Declaration and so on)
-- `b` is a type of the result by which the dependencies are going to be represented (like Set Int, or [Set Int], ...)
class Depends a b where
  depends'on :: a -> Map.Map String Int -> b