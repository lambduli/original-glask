module Compiler.Analysis.Syntactic.MethodAnnotations where

import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )

import Compiler.Syntax.Declaration ( Declaration(Class) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate(Is'In) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V(T'V), Type(..) )
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations


{- This module collects all type annotations within the list of Declarations -}

extract :: [Declaration] -> [(Name, Sigma'Type, Name)]
extract declarations = concat $ mapMaybe collect declarations


{-  NOTE: I am using the `collect` from the Annotations module.
          That way it will collect all annotations from within the class declaration list for me.
-}
collect :: Declaration -> Maybe [(Name, Sigma'Type, Name)]
collect (Class cl'name t@(T'V var'name k) _ declarations)
  -- TODO: I need to qualify the method type with the Predicate stating that the class variable is in the current class
  = return $ mapMaybe (fmap qualify'method . Annotations.collect) declarations
    where
      -- qualify'method :: (Name, Sigma'Type) -> (Name, Sigma'Type, Name)
      qualify'method (name, T'Forall tvs (context :=> type'))
        = (name, T'Forall tvs ((Is'In cl'name (T'Var t) : context) :=> type'), var'name)
collect _
  = Nothing
