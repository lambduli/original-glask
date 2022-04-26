module Compiler.TypeSystem.Type.Infer.Match where


import Compiler.Syntax.Match ( Match )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Type ( Type )


import Compiler.TypeSystem.Infer ( Infer, Type'Check )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Expected ( Expected )
import Compiler.TypeSystem.Actual ( Actual )


-- infer'match :: Match -> Expected Type -> Type'Check ([Actual Type], Actual Type, [Predicate], [Predicate])
infer'match :: Match -> Expected Type -> Type'Check (Match, Actual Type, [Predicate], [Predicate])
