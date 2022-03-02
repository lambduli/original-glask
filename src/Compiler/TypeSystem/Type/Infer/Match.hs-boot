module Compiler.TypeSystem.Type.Infer.Match where


import Compiler.Syntax


import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint


infer'match :: Match -> Infer ([Type], Type, [Predicate], [Predicate], [Constraint Type])
