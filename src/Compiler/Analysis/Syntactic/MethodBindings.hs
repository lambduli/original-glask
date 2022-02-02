module Compiler.Analysis.Syntactic.MethodBindings where

import qualified Data.Map.Strict as Map
import Data.Maybe

import Compiler.Syntax

import qualified Compiler.Analysis.Syntactic.Bindings as Bindings



extract :: [Declaration] -> [(Name, Bind'Group, Type)]
extract declarations = concat $ mapMaybe collect declarations


{-  NOTE: For now, I am expecting only Instances to contain actual implementations. -}
collect :: Declaration -> Maybe [(Name, Bind'Group, Type)]
collect (Instance (_ :=> (Is'In _ type')) declarations)
  = return $ mapMaybe ((fmap (\ (name, b'g) -> (name, b'g, type'))) . Bindings.collect) declarations
collect _ = Nothing
