module Compiler.TypeSystem.Type.Infer.Declaration where


import Compiler.Syntax.Declaration ( Declaration )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type )

import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Infer ( Infer, Type'Check )
import Compiler.TypeSystem.Assumption ( Assumption )
import Compiler.Syntax.Type ( Sigma'Type )


infer'decls :: [Declaration] -> Type'Check ([Predicate], [Assumption Sigma'Type])
