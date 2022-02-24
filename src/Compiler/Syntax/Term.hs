module Compiler.Syntax.Term
  ( Term'Id(..)
  , Term'Expr(..)
  , Term'Pat(..)
  , Term'Type(..)
  , Term'Decl(..), Term'Constr'Decl(..), Term'Pred(..) ) where


import Compiler.Syntax.Term.Identifier
import {-# SOURCE #-} Compiler.Syntax.Term.Expression
import Compiler.Syntax.Term.Pattern
import {-# SOURCE #-} Compiler.Syntax.Term.Type
import Compiler.Syntax.Term.Declaration
import Compiler.Syntax.Term.Predicate


{-
Ignoring module declaration for now.

I have top level declarations
  fixity declarations
  data declarations
  type alias declarations
  type annotations
  match groups

So the question is, how to represent these.
Should I, in some cases go straight for the final representation which will be used for type analysis?
In case of fixity, data, type, and annotation declaration I think I could.
Of course there's the interesting point about annotations.
Jones represents explicitly annotated bindings differently.
But what if I want to have explicit annotations in the expressions?
I think I would need to handle those two cases differently.
So what if I do it like this:
  Type annotations ale collected, and after succesfull pass of semantic analysis,
  those are embedded in the corresponding bindings, changing the original Expression to
  annotationed Expression. I will read that section in the THIH again and see if it should work,
  but I feel like there's equivalence between stuff like:

  foo :: Int -> Int
  foo x = x

  and

  foo = (\ x -> x) :: Int -> Int

  I think like this equivalence comes directly from the equivalence of the same example with all types stripped away.

  So it should work fine and it shouldn't create any problems. Hopefully anyway.


Then what about Match Groups?
Those are different, because in the Term representation they are going to be modeled
in simplified form:
  Application is just a sequence of Expressions
  Patterns are just applications. They will later be "fixed/reordered" and then transformed into actual Patterns in the correct sense.

-}
