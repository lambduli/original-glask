{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Analysis.TypeSystem.Solver.Inject where


import Compiler.Syntax


class Inject a b where
  inject :: a -> b


instance Inject T'V Type where
  inject = T'Var

instance Inject Name Kind where
  inject = K'Var
