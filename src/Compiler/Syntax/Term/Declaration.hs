{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.Term.Declaration where


import qualified Data.Set as Set


import {-# SOURCE #-} Compiler.Syntax.Term.Expression ( Term'Expr )
import {-# SOURCE #-} Compiler.Syntax.Term.Type ( Term'Type )
import Compiler.Syntax.Term.Pattern ( Term'Pat )
import Compiler.Syntax.Term.Predicate ( Term'Pred )
import Compiler.Syntax.Term.Identifier ( Term'Id )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Fixity ( Fixity )
import Compiler.Syntax.Associativity ( Associativity )

import Compiler.TypeSystem.Solver.Substitutable ( Term(..) )


data Term'Decl
  = Binding Term'Pat Term'Expr                -- id x = x
  -- I may need to add information about whether it is explicitly typed or not

  | Signature Name ([Term'Pred], Term'Type)   -- id :: a -> a
  | Data'Decl Name [Name] [Term'Constr'Decl]  -- Data type declaration -- name type'params list'of'consturctors'with'params
  | Type'Alias Name [Name] Term'Type          -- type String = List Char
  | Fixity Fixity Associativity Int Name      -- infix 5 +
  | Class'Decl Name Name [Term'Pred] [Term'Decl]   -- class (Super1 a, ... , SuperN a) ==> Name a where { list of Signatures }
  --    cname parname supers     signatures
  | Instance ([Term'Pred], Term'Pred) [Term'Decl]             -- instance ... where { list of Bindings }
  deriving (Eq)


instance Show Term'Decl where
  show (Binding term'pat term'expr)
    = "[[Binding]]    " ++ show term'pat ++ " = " ++ show term'expr

  show _ = "Not Implemented: Show for Term'Decl"


data Term'Constr'Decl
  = Con'Decl Name [Term'Type]
  | Con'Record'Decl Name [(Name, Term'Type)]
  deriving (Eq)


-- TODO:  Implement later
instance Term Term'Id Term'Decl where
  free'vars _ = Set.empty
