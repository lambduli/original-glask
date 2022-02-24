{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Type where

import Compiler.Syntax.Term.Identifier


import Compiler.TypeSystem.Solver.Substitutable


data Term'Type


instance Term Term'Id Term'Type


instance Eq Term'Type
