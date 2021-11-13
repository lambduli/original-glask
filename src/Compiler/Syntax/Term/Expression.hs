module Compiler.Syntax.Term.Expression where


import Compiler.Syntax.Name
import Compiler.Syntax.Literal
import Compiler.Syntax.Term.Declaration
import Compiler.Syntax.Term.Identifier
import Compiler.Syntax.Term.Type
import Compiler.Syntax.Term.Pattern
import Compiler.Syntax.Term.Predicate

-- TODO: add Positions to specific alternatives for better error reporting
data Term'Expr
  = Term'E'Id Term'Id
  | Term'E'Op Term'Id -- for +, : or `SomeConstr`
  | Term'E'Lit Literal
  | Term'E'Abst Term'Pat Term'Expr
  | Term'E'App [Term'Expr]
  | Term'E'Tuple [Term'Expr]
  | Term'E'List [Term'Expr]
  | Term'E'Arith'Seq Term'Expr (Maybe Term'Expr) Term'Expr
  | Term'E'If Term'Expr Term'Expr Term'Expr
  | Term'E'Let [Term'Decl] Term'Expr
  | Term'E'Ann Term'Expr ([Term'Pred], Term'Type)
  | Term'E'Case Term'Expr [(Term'Pat, Term'Expr)]
  | Term'E'Labeled'Constr Name [(Name, Term'Expr)]
  | Term'E'Labeled'Update Term'Expr [(Name, Term'Expr)]
  deriving (Eq)


instance Show Term'Expr where
  show (Term'E'Lit lit) = show lit

  show _ = "Not Implemented: Show for Term'Expr"