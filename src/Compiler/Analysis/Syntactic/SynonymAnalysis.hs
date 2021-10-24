module Compiler.Analysis.Syntactic.SynonymAnalysis where

import qualified Data.Map.Strict as Map
import Data.Maybe


import Compiler.Syntax.Name
import Compiler.Syntax.Term

import Compiler.Analysis.Syntactic.SynonymEnv


{-
  This module will construct a Synonym Environment. That is - a map, from a Name to an Int.
  It is usefull to know what is an arity of every and each type synonym declaration.
  Because type synonyms must be fully applied - as per syntactic rule - for it to be a predicative. (I think that is what it's about.)
-}

analyze :: [Term'Decl] -> Synonym'Env
analyze decls = Map.fromList $ mapMaybe collect decls


collect :: Term'Decl -> Maybe (Name, Int)
collect (Type'Alias syn'name params _)
  = Just (syn'name, length params)
collect _
  = Nothing
