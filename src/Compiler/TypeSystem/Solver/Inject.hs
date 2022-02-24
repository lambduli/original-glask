{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.TypeSystem.Solver.Inject where


import Compiler.Syntax
import Compiler.Syntax.Type


class Inject a b where
  inject :: a -> b


instance Inject T'V Type where
  inject = T'Var

instance Inject Name Kind where
  inject = K'Var
