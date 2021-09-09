module Compiler.Analysis.TypeSystem.Type.Infer.Expression where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Infer


infer'expr :: Expression -> Infer ([Predicate], Type, [Constraint Type], [Constraint Kind])
