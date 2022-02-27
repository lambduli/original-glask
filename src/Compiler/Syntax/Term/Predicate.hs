{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Predicate where


import Compiler.Syntax.Name ( Name )

import Compiler.TypeSystem.Solver.Substitutable ( Term(..) )
import Compiler.Syntax.Term.Identifier ( Term'Id )
import {-# SOURCE #-} Compiler.Syntax.Term.Type ( Term'Type )

data Term'Pred = Is'In Name Term'Type
  deriving (Eq)


instance Term Term'Id Term'Pred where
  free'vars (Is'In name term'type)
    = free'vars term'type
