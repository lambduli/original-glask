module Compiler.Analysis.ConstructorAnalysis where


import qualified Data.Map.Strict as Map
import Data.Maybe


import Compiler.Syntax.Name
import Compiler.Syntax.Term

import Compiler.Analysis.ConstrEnv


analyze :: [Term'Decl] -> Constr'Env
analyze decls = Map.fromList $ concat $ mapMaybe collect decls


collect :: Term'Decl -> Maybe [(Name, Constr'Info)]
collect (Data'Decl _ _ t'constr'decls)
  = Just $ map collect'' t'constr'decls
    where collect'' :: Term'Constr'Decl -> (Name, Constr'Info)
          collect'' (Con'Decl name types) = (name, Constr)
          collect'' (Con'Record'Decl name field'assigns) = (name, Record { fields = map fst field'assigns })
collect _
  = Nothing
