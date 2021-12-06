{-# LANGUAGE TupleSections #-}

module Compiler.Analysis.Syntactic.Constructors where


import qualified Data.Map.Strict as Map
import Data.Maybe


import Compiler.Syntax.Name
import Compiler.Syntax.Term

import Compiler.Analysis.Syntactic.ConstrEnv
import Compiler.Analysis.Syntactic.FieldEnv


extract :: [Term'Decl] -> (Constr'Env, Field'Env)
extract decls = (constr'env, field'env)
  where
    constr'env = Map.fromList $ concat $ mapMaybe collect decls
    
    {-  From all the entries in the `constr'env` I only consider bindings of type `Record { fields :: [Name] }`.
        For such bindings I assign each field name the Constr'Info they are tied to.
        All of those new bindings are then concatenated into a singular collection and transformed into a Map aka Field'Env. -}
    field'env = Map.fromList $ concat [ map (, constr) fields | (constr'name, constr@Record{ fields = fields }) <- entries ]
      where
        entries = Map.toList constr'env


collect :: Term'Decl -> Maybe [(Name, Constr'Info)]
collect (Data'Decl _ _ t'constr'decls)
  = Just $ map collect'' t'constr'decls
    where collect'' :: Term'Constr'Decl -> (Name, Constr'Info)
          collect'' (Con'Decl name types) = (name, Constr name)
          collect'' (Con'Record'Decl name field'assigns) = (name, Record { name = name, fields = map fst field'assigns })
collect _
  = Nothing
