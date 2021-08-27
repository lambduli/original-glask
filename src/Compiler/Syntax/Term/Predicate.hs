module Compiler.Syntax.Term.Predicate where


import Compiler.Syntax.Name

import Compiler.Syntax.Term.Type


data Term'Pred = Is'In Name Term'Type
  deriving (Eq)
