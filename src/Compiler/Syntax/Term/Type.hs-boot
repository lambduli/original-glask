{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Type where

import Compiler.Syntax.Term.Identifier ( Term'Id )


import Compiler.TypeSystem.Solver.Substitutable ( Term )


data Term'Type


instance Term Term'Id Term'Type


instance Eq Term'Type
