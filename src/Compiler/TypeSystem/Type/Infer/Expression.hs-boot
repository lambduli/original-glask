module Compiler.TypeSystem.Type.Infer.Expression where


import Compiler.Syntax.Expression ( Expression )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type, Rho'Type )

import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Infer ( Infer, Type'Check )
import Compiler.TypeSystem.Expected ( Expected )
import Compiler.TypeSystem.Actual ( Actual )


infer'expr :: Expression -> Expected Rho'Type -> Type'Check (Expression, [Predicate], Actual Rho'Type)

