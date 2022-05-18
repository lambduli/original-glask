module Compiler.Analysis.Syntactic.Fixity where


import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )


import Compiler.Syntax.Name ( Name )

import Compiler.Syntax.Term.Declaration ( Term'Decl(Fixity) )

import Compiler.Analysis.Syntactic.FixityEnv ( Fixity'Env, Fixity'Info )
import Compiler.Syntax.Fixity ( Fixity(Infix) )
import Compiler.Syntax.Associativity as Associativity


extract :: [Term'Decl] -> Fixity'Env
extract decls = Map.fromList $ (":", (Infix, Associativity.Right, 5)) : mapMaybe collect decls


{- NOTE:
  I only collect the top level Fixity declarations.
  This way I should be able to translate/parse local bindings too, I will just call `analyze` again -}
collect :: Term'Decl -> Maybe (Name, Fixity'Info)
collect (Fixity fixity associativity level name)
  = Just (name, (fixity, associativity, level))
collect _
  = Nothing