module Compiler.Analysis.Syntactic.Annotations where

import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )

import Compiler.Syntax.Declaration ( Declaration(Signature) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Signature ( Signature(T'Signature) )
import Compiler.Syntax.Type ( Sigma'Type )


{- This module collects all type annotations within the list of Declarations -}

extract :: [Declaration] -> Map.Map Name Sigma'Type
extract declarations = Map.fromList $ mapMaybe collect declarations


collect :: Declaration -> Maybe (Name, Sigma'Type)
collect (Signature (T'Signature name sigma'type))
  = Just (name, sigma'type)
collect _
  = Nothing
