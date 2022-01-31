module Compiler.Analysis.TypeSystem.Type.Infer.Declaration where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Assumption


infer'decls :: [Declaration] -> Infer ([Predicate], [Assumption Scheme], [Constraint Type], [Constraint Kind])
