module Compiler.Analysis.Semantic.Data where


import Data.Bifunctor (Bifunctor(second))
import Data.Maybe (mapMaybe)


import Compiler.Syntax.Declaration (Declaration (Data'Decl))
import Compiler.Syntax (Name, Scheme, T'C (T'C), Type(..), T'V(..), Constr'Decl(..))

import Compiler.TypeSystem.Utils.Infer (close'over, qualify)
import Compiler.TypeSystem.Type.Constants (type'fn)


{-  This module collects typing assumptions about data constructors. It also collects types of all record fields/getters. -}


extract :: [Declaration] -> [(Name, Scheme)]
extract declarations = concat $ mapMaybe collect declarations


collect :: Declaration -> Maybe [(Name, Scheme)]
collect (Data'Decl tc@(T'C n k) t'params constr'decls)
  = let res'type = foldl T'App (T'Con tc) $ map T'Var t'params

        con'type :: Constr'Decl -> [(Name, Type)]
        con'type (Con'Decl c'name types)
          = [(c'name, foldr type'fn res'type types)]
        con'type (Con'Record'Decl c'name fields)
          = let getter'types = map (second (type'fn res'type)) fields
                c'type = (c'name, foldr (type'fn . snd) res'type fields)
            in c'type : getter'types

        constr'types = concatMap con'type constr'decls
    in Just $ map (second (close'over . qualify)) constr'types

collect _ = Nothing
