module Compiler.Analysis.Syntactic.Fixity where


import qualified Data.Map.Strict as Map
import Data.Maybe


import Compiler.Syntax.Name

import Compiler.Syntax.Term

import Compiler.Analysis.Syntactic.FixityEnv ( Fixity'Env, Fixity'Info )


extract :: [Term'Decl] -> Fixity'Env
extract decls = Map.fromList $ mapMaybe collect decls


{- NOTE:
  I only collect the top level Fixity declarations.
  This way I should be able to translate/parse local bindings too, I will just call `analyze` again -}
collect :: Term'Decl -> Maybe (Name, Fixity'Info)
collect (Fixity fixity associativity level name)
  = Just (name, (fixity, associativity, level))
collect _
  = Nothing