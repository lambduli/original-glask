module Compiler.Analysis.Syntactic.MethodAnnotations where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Compiler.Syntax

import qualified Compiler.Analysis.Syntactic.Annotations as Annotations


{- This module collects all type annotations within the list of Declarations -}

extract :: [Declaration] -> [(Name, Qualified Type, Name)]
extract declarations = concat $ mapMaybe collect declarations


{-  NOTE: I am using the `collect` from the Annotations module.
          That way it will collect all annotations from within the class declaration list for me.
-}
collect :: Declaration -> Maybe [(Name, Qualified Type, Name)]
collect (Class _ var'name _ declarations)
  = return $ mapMaybe ((fmap (\ (name, q't) -> (name, q't, var'name))) . Annotations.collect) declarations
collect _
  = Nothing
