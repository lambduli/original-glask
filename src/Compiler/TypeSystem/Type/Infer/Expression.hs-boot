module Compiler.TypeSystem.Type.Infer.Expression where


import Compiler.Syntax

import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Infer


infer'expr :: Expression -> Infer ([Predicate], Type, [Constraint Type])
