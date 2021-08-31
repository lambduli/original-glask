module Compiler.Analysis.Syntactic.FixityAnalysis where


import qualified Data.Map.Strict as Map
import Data.Maybe


import Compiler.Syntax.Name

import Compiler.Syntax.Term

import Compiler.Analysis.Syntactic.FixityEnv


analyze :: [Term'Decl] -> Fixity'Env
analyze decls = Map.fromList $ mapMaybe collect decls


{- NOTE:
  I only collect the top level Fixity declarations.
  This way I should be able to translate/parse local bindings too, I will just call `analyze` again -}
collect :: Term'Decl -> Maybe (Name, Fixity'Info)
collect (Fixity fixity level name)
  = Just (name, (fixity, level))
collect _
  = Nothing