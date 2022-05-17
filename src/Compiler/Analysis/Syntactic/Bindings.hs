module Compiler.Analysis.Syntactic.Bindings where

import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )

import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name) )
import Compiler.Syntax.Declaration ( Declaration(Binding) )
import Compiler.Syntax.Name ( Name )



extract :: [Declaration] -> Map.Map Name Bind'Group
extract declarations = Map.fromList $ mapMaybe collect declarations


collect :: Declaration -> Maybe (Name, Bind'Group)
collect (Binding bg@Bind'Group{ name = name }) = Just (name, bg)
collect _ = Nothing
