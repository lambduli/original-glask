module Compiler.TypeSystem.Type.Infer.Declaration where


import Compiler.Syntax

import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Assumption


infer'decls :: [Declaration] -> Infer ([Predicate], [Assumption Scheme], [Constraint Type], [Constraint Kind])
