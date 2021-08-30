module Compiler.Syntax.ToAST.SemanticError where


import Compiler.Syntax.Type


newtype Semantic'Error
  = Unbound'T'Var Type
