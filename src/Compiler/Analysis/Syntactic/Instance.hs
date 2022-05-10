module Compiler.Analysis.Syntactic.Instance where


import Data.Maybe ( mapMaybe )


import Compiler.Syntax.Declaration ( Declaration(Instance) )
import Compiler.Syntax.Instance ( Instance )


extract :: [Declaration] -> [(Instance, [Declaration])]
extract decls = mapMaybe collect decls
    

collect :: Declaration -> Maybe (Instance, [Declaration])
collect (Instance inst decls) = Just (inst, decls)
collect _ = Nothing
