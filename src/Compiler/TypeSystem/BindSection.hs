{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.TypeSystem.BindSection where


import {-# SOURCE #-} Compiler.Syntax.Type ( M'V, Type )

import Compiler.TypeSystem.Binding ( Explicit, Implicit )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(..) )


type Bind'Section = ([Explicit], [[Implicit]])


-- instance Substitutable M'V Bind'Section Type where
--   apply = undefined
