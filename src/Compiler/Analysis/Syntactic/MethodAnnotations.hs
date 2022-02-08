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
collect (Class cl'name t@(T'V var'name k) _ declarations)
  -- TODO: I need to qualify the method type with the Predicate stating that the class variable is in the current class
  = return $ mapMaybe (fmap (\ (name, context :=> type') -> (name, (Is'In cl'name (T'Var t) : context) :=> type', var'name)) . Annotations.collect) declarations
collect _
  = Nothing
