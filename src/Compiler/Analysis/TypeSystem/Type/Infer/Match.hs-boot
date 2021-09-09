module Compiler.Analysis.TypeSystem.Type.Infer.Match where


import Compiler.Syntax


import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint


infer'match :: Match -> Infer ([Type], Type, [Predicate], [Predicate], [Constraint Type], [Constraint Kind])
