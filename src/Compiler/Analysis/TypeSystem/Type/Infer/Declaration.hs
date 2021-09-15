module Compiler.Analysis.TypeSystem.Type.Infer.Declaration where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Assumption


import {-# SOURCE #-} Compiler.Analysis.TypeSystem.Type.Infer.Expression


--
-- Most likely, this function will represent a function tiProgram from the THIH
--
infer'decls :: [Declaration] -> Infer ([Predicate], [Assumption Scheme], [Constraint Type], [Constraint Kind])
infer'decls decls = do
  -- transform [Declaration] into [Binding]


  undefined



{- TODO:

  First I need to split the list of declarations into a Bind'Groups
  where type Bind'Groups = ([Expl], [[Impl]])
  where type Expl = (Name, Scheme, [Alt])
        type Impl = (Name, [Alt])

  To do that, I will first need to process the list of declarations like this:
    - find and group all equations for the same thing into a single Bind'Group
    - find all explicit annotations and connect them with the Bind'Group
  It might be better to do the first thing even before type analysis. It feels more like to'AST kind of thing.

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


  Also what I don't seem to understand - the paper mentions that the Program could be defined like this:
  type Program = [Bind'Groups]

  But that then means that I will work with many instances of what I have described above.
  I don't really get why.
  What would be the point spliting the (top level) declarations into many (explicits, implicits's)?

  I don't even see how it would behave any differently then my prefered way.
-}
