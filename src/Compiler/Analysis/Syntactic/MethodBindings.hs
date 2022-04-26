module Compiler.Analysis.Syntactic.MethodBindings where


import qualified Data.Map.Strict as Map
import Data.Maybe ( mapMaybe )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Declaration ( Declaration(Instance) )
import Compiler.Syntax.BindGroup ( Bind'Group )
import Compiler.Syntax.Predicate ( Predicate(Is'In) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Type ( Type )

import qualified Compiler.Analysis.Syntactic.Bindings as Bindings



extract :: [Declaration] -> [(Name, Bind'Group, Type, Name)]
extract declarations = concat $ mapMaybe collect declarations


{-  NOTE: For now, I am expecting only Instances to contain actual implementations. -}
collect :: Declaration -> Maybe [(Name, Bind'Group, Type, Name)] -- method name, bind group, instance type, class of the instance
collect (Instance (_ :=> (Is'In cl'n type')) declarations) = do
  let bindings  = mapMaybe ((fmap (\ (name, b'g) -> (name, b'g, type'))) . Bindings.collect) declarations
      complete  = map (\ (m'name, b'g, t) -> (m'name, b'g, t, cl'n) ) bindings
  return complete
collect _ = Nothing
