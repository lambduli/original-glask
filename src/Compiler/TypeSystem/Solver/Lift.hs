{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.TypeSystem.Solver.Lift where


import Compiler.Syntax.Kind ( Kind(K'Var) )
import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'V', Type(T'Var', T'Meta), M'V )


class Lift a b where
  lift :: a -> b


instance Lift T'V' Type where
  lift = T'Var'


instance Lift M'V Type where
  lift = T'Meta


instance Lift Name Kind where
  lift = K'Var
