{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Expression where


import Compiler.Syntax.Term.Identifier ( Term'Id )

import Compiler.TypeSystem.Solver.Substitutable ( Term )


data Term'Expr


instance Eq Term'Expr


instance Show Term'Expr


instance Term Term'Id Term'Expr
