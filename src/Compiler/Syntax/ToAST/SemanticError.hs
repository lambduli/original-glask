module Compiler.Syntax.ToAST.SemanticError where


import Compiler.Syntax.Name
import Compiler.Syntax.Type
import Compiler.Syntax.Term.Expression


data Semantic'Error
  = Unbound'T'Var Type
  | Not'In'Scope'Data Name
  | Wrong'Fields Name [Name]
  | Uninitialized'Fields Name [Name]
  | Empty'Record'Update Term'Expr
  | Not'In'Scope'Field Name
  | No'Constructor'Has'All'Fields [Name]
