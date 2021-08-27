module Compiler.Syntax.Term
  ( Term'Id(..)
  , Term'Expr(..)
  , Term'Pat(..)
  , Term'Type(..)
  , Term'Decl(..), Term'Constr'Decl(..), Term'Pred(..) ) where


import Compiler.Syntax.Term.Identifier
import {-# SOURCE #-} Compiler.Syntax.Term.Expression
import Compiler.Syntax.Term.Pattern
import Compiler.Syntax.Term.Type
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

---------
Takze co presne musim reprezentovat v Term?
No akorat to, co by jinak porusovalo pravidla validity pro Expression
Tak to me napada akorat Application - v prvni fazi to bude list expressions ve finale to bude jenom binarni operace.

Stejne to plati pro typovy aplikace? No pokud budu chtit mit veci jako infix typovy operatory tak jo.
Jinak se obejdu bez toho.
-}


-- data Term'Id
--   = Term'Id'Var Name
--   | Term'Id'Const Name
--   deriving (Eq)


-- -- TODO: add Positions to specific alternatives for better error reporting
-- data Term'Expr
--   = Term'E'Id Term'Id
--   -- | Term'E'Const Term'Id
--   | Term'E'Op Term'Id -- for +, : or `SomeConstr`
--   | Term'E'Lit Literal
--   | Term'E'Abst Term'Pat Term'Expr
--   | Term'E'App [Term'Expr]
--   | Term'E'Tuple [Term'Expr]
--   | Term'E'List [Term'Expr]
--   | Term'E'Arith'Seq Term'Expr (Maybe Term'Expr) Term'Expr
--   | Term'E'If Term'Expr Term'Expr Term'Expr
--   | Term'E'Let [Term'Decl] Term'Expr
--   | Term'E'Ann Term'Expr ([Term'Pred], Term'Type)
--   | Term'E'Case Term'Expr [(Term'Pat, Term'Expr)]
--   | Term'E'Labeled'Constr Name [(Name, Term'Expr)]
--   | Term'E'Labeled'Update Term'Expr [(Name, Term'Expr)]
--   deriving (Eq)


-- data Term'Pat
--   = Term'P'Id Term'Id
--   -- | Term'P'Const Term'Id
--   | Term'P'Op Term'Id
--   | Term'P'Lit Literal
--   | Term'P'App [Term'Pat]
--   | Term'P'Labeled Name [(Name, Term'Pat)] -- can be desugared to Term'P'App with correct order of field values
--   | Term'P'Tuple [Term'Pat] -- will be able to desugar Term'P'App
--   | Term'P'List [Term'Pat] -- will be able to desugar - same way
--   | Term'P'As Name Term'Pat -- named pattern
--   | Term'P'Wild
--   deriving (Eq)


-- data Term'Type
--   = Term'T'Id Term'Id
--   | Term'T'Tuple [Term'Type]
--   | Term'T'List Term'Type
--   | Term'T'Arrow [Term'Type]
--   | Term'T'App [Term'Type]
--   deriving (Eq)


-- tt'arr :: Term'Type
-- tt'arr = Term'T'Id $ Term'Id'Const "(->)"


-- infixr 0 -->

-- (-->) :: Term'Type -> Term'Type -> Term'Type
-- domain --> codomain = Term'T'App [Term'T'App [tt'arr, domain], codomain]
