module Compiler.TypeSystem.Solver.InvolvesPoly where


import Data.List ( any )


import {-# SOURCE #-} Compiler.Syntax.Type ( Type(..) )


{-  INVARIANT:  Assumes that the argument is in weak-prenex-form  -}
involves'poly :: Type -> Bool
involves'poly (T'Var' _) = False
involves'poly (T'Meta _) = False
involves'poly (T'Con _) = False
involves'poly (T'Tuple types) = any involves'poly types
involves'poly (T'App left right) = involves'poly left || involves'poly right
involves'poly (T'Forall _ _) = True
