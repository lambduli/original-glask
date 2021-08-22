module Compiler.Syntax.Term.Type where


import Compiler.Syntax.Term.Identifier


data Term'Type
  = Term'T'Id Term'Id
  | Term'T'Tuple [Term'Type]
  | Term'T'List Term'Type
  | Term'T'Arrow [Term'Type]
  | Term'T'App [Term'Type]
  deriving (Eq)
