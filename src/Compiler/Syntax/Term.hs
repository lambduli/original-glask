module Compiler.Syntax.Term where


import Compiler.Syntax.Literal
import Compiler.Syntax.Qualified
import Compiler.Syntax.Type
import Compiler.Syntax.Name


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


-- TODO: add Positions to specific alternatives for better error reporting
data Term
  = Term'Var Name
  | Term'Const Name
  | Term'Op Name
  | Term'Lit Literal
  | Term'Abst Name Term
  | Term'App [Term]
  | Term'Tuple [Term]
  | Term'If Term Term Term
  | Term'Let [(Term, Term)] Term
  | Term'Ann Term (Qualified Type) -- should it be Qualified or not?
  | Term'Case Term [(Term, Term)]
  deriving (Eq)
