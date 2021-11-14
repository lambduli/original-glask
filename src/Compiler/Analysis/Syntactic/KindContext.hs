module Compiler.Analysis.Syntactic.KindContext where

import qualified Data.Map.Strict as Map

import Compiler.Syntax.Kind


type Kind'Context = Map.Map String Kind

init'kind'context
  = Map.fromList known'type'constructors


-- that means primitive types like Int, Char, Tuple, Unit, List, Bool, (->)
{-  This list needs to contain Kinds for all primitive type constructors. -}
-- Int
-- Char
-- Bool
-- []
-- () and all other tuples
-- (->)
known'type'constructors =
  [ ("Int", K'Star)
  , ("Char", K'Star)
  , ("Bool", K'Star)
  , ("[]", K'Star)
  , ("()", K'Star)
  , ("(,)", K'Star)
  , ("(->)", K'Star) ]
