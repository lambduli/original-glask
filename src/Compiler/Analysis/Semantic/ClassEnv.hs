module Compiler.Analysis.Semantic.ClassEnv where

import qualified Data.Map.Strict as Map
import Data.Bifunctor ( Bifunctor(second) )

import Compiler.Syntax.Name ( Name )
import qualified Compiler.Syntax.Declaration as Syntax
import Compiler.Syntax.Instance ( Instance )
import Compiler.Syntax.Predicate ( Predicate(..) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Declaration ( Declaration(..) )

import Compiler.TypeSystem.Class ( Class, Supers )
import Compiler.TypeSystem.ClassEnv ( Class'Env(..) )
import Compiler.TypeSystem.Type.Constants ( t'Int, t'Double )


{- This module builds a Class'Env for the Type Inference
    it needs [Declaration] and produces Class'Env
-}


extract :: [Declaration] -> Class'Env
extract declarations = Class'Env{ classes = classes, defaults = [t'Int, t'Double] }
  where
    is'class :: Declaration -> Bool
    is'class (Class'Decl _) = True
    is'class _ = False

    is'instance :: Declaration -> Bool
    is'instance (Instance _ _) = True
    is'instance _ = False

    cls'lst :: [(Name, (Supers, [Instance]))]
    cls'lst = map (\ (Class'Decl (Syntax.Class name var preds decls)) -> (name, (supers preds, []))) $ filter is'class declarations

    supers :: [Predicate] -> [Name]
    supers = map (\ (Is'In name type') -> name) -- where type' is assumed to only ever be a Type Variable

    instances :: [Instance]
    instances = map (\ (Instance qual'pred _) -> qual'pred) $ filter is'instance declarations

    build'class'map :: [Instance] -> Map.Map Name Class
    build'class'map instances = go instances $ Map.fromList cls'lst
      where
        go :: [Instance] -> Map.Map Name Class -> Map.Map Name Class
        go [] classes = classes
        go (inst@(_ :=> (Is'In name type')) : insts) classes = go insts $ Map.adjust (second (inst :)) name classes
        -- where type' is assumed to only ever be a Type Variable

    classes = build'class'map instances
