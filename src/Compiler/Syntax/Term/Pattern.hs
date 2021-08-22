module Compiler.Syntax.Term.Pattern where


import Compiler.Syntax.Name
import Compiler.Syntax.Literal
import Compiler.Syntax.Term.Identifier


data Term'Pat
  = Term'P'Id Term'Id
  -- | Term'P'Const Term'Id
  | Term'P'Op Term'Id
  | Term'P'Lit Literal
  | Term'P'App [Term'Pat]
  | Term'P'Labeled Name [(Name, Term'Pat)] -- can be desugared to Term'P'App with correct order of field values
  | Term'P'Tuple [Term'Pat] -- will be able to desugar Term'P'App
  | Term'P'List [Term'Pat] -- will be able to desugar - same way
  | Term'P'As Name Term'Pat -- named pattern
  | Term'P'Wild
  deriving (Eq)
