module Compiler.TypeSystem.Type.Infer.Declaration where


import Compiler.Syntax

import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Assumption


import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Expression


{-  TODO: I inteded to use this function to infer list of declarations like `let` blocks.
          I think it would be best to utilize the infrastructure I already have - the one from the THIH.
          This function might be used just to utilize it.
 -}

infer'decls :: [Declaration] -> Infer ([Predicate], [Assumption Scheme], [Constraint Type], [Constraint Kind])
infer'decls = undefined

{- TODO:
  NOW:
  `tiBindGroup` takes the Bind'Groups
  it first puts all the type bindings for the explicitly annotated declarations into the typing context
  then it uses another function to effectively go over the list of implicitly typed declarations and infer
  the whole group with the use of a function `tiImpls`.
  That function does the usual stuff - but there IS an interesting part of it.
  It infers the types, accumulates the Type Constraints and then it solves them.
  It then applies the obtained substitution to mostly everything, to be specific
  it applies it to the typing context (Assumptions) which it then returns.
  This way it is ensured that only the type inference for the small group of implicittly typed bidings
  will get the access to the original assumptions stating that each variable declared is of any type at all
  (via a fresh type variable not closed under its type scheme).
  That unprotected type variable is then replaced in the process of the substitution with the infered type.

  When all un-annotated bindings are infered, we take all the assumptions
    (the original ones, the ones obtained by registering all the annotated bindings, and the ones
    obtained by infering the un-annotated bindings), and we infer the types for the explicitly typed
  (annotated) bindings. This only produces a list of Predicates - qualificators.
  I am not sure what for.

  I think it is because after all of the Predicates from the group are collected, we simplify them
  and create a new substitution from that -> then we apply that substitution to the typing context and
  obtain a "better version of the type qualificators".

  I need to figure out whether the simplification (`reduce`) is strictly necessary.

-}
