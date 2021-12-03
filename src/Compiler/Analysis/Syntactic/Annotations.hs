module Compiler.Analysis.Syntactic.Annotations where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Compiler.Syntax


{- This module collects all type annotations within the list of Declarations -}

extract :: [Declaration] -> Map.Map Name (Qualified Type)
extract declarations = Map.fromList $ mapMaybe collect declarations


collect :: Declaration -> Maybe (Name, Qualified Type)
collect (Signature (T'Signature name qual'type))
  = Just (name, qual'type)
collect _
  = Nothing
