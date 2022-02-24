module Compiler.Syntax.Term.Expression where


import Compiler.Syntax.Name
import Compiler.Syntax.Literal
import Compiler.Syntax.Term.Declaration
import Compiler.Syntax.Term.Identifier
import {-# SOURCE #-} Compiler.Syntax.Term.Type
import Compiler.Syntax.Term.Pattern
import Compiler.Syntax.Term.Predicate

import Compiler.TypeSystem.Solver.Substitutable

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


instance Term Term'Expr where
  free'vars (Term'E'Id Term'Id)
    = Set.empty
  
  free'vars (Term'E'Op Term'Id)
    = Set.empty
  
  free'vars (Term'E'Lit Literal)
    = Set.empty
  
  free'vars (Term'E'Abst _ t'expr)
    = free'vars t'expr
  
  free'vars (Term'E'App t'exprs)
    = free'vars t'exprs
  
  free'vars (Term'E'Tuple t'exprs)
    = free'vars t'exprs
  
  free'vars (Term'E'List t'exprs)
    = free'vars t'exprs
  
  free'vars (Term'E'Arith'Seq t'e1 (Just t'e2) t'e3)
    = free'vars [t'e1, t'e2, t'e3]
  
  free'vars (Term'E'Arith'Seq t'e1 Nothing t'e2)
    = free'vars [t'e1, t'e2]
  
  free'vars (Term'E'If t'cond t'then t'else)
    = free'vars [t'cond, t'then, t'else]
  
  free'vars (Term'E'Let t'decls t'expr)
    = free'vars t'decls `Set.union` free'vars t'expr
  
  free'vars (Term'E'Ann t'expr (t'preds, t'type))
    = free'vars t'expr `Set.union` free'vars t'preds `Set.union` free'vars t'type
  
  free'vars (Term'E'Case Term'Expr [(Term'Pat, Term'Expr)])
    = undefined
  
  free'vars (Term'E'Labeled'Constr Name [(Name, Term'Expr)]) = undefined
  
  free'vars (Term'E'Labeled'Update Term'Expr [(Name, Term'Expr)]) = undefined