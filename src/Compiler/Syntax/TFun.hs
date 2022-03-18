{-# LANGUAGE PatternSynonyms #-}

{-   This module exists because it is not possible to forward declare pattern synonyms inside the hs-boot files.  -}

module Compiler.Syntax.TFun (pattern T'Fun) where


import {-# SOURCE #-} Compiler.Syntax.Type ( Type(..), T'C(T'C) )

import Compiler.Syntax.Kind ( Kind(K'Star, K'Arr) )


{-  T'Fun represents this pattern: a -> b  where `a` is considered `argument` and `b` `result`, -}
pattern T'Fun :: Type -> Type -> Type
pattern T'Fun argument result = (T'App (T'App Fun'Constr argument) result)


pattern Fun'Constr :: Type
pattern Fun'Constr = T'Con (T'C "(->)" (K'Star `K'Arr` (K'Star `K'Arr` K'Star)))
