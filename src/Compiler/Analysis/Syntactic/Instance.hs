module Compiler.Analysis.Syntactic.Instance where


import Data.Maybe ( mapMaybe )


import Compiler.Syntax.Declaration ( Declaration(Instance) )
import Compiler.Syntax.Instance ( Instance )


extract :: [Declaration] -> [Instance]
extract decls = mapMaybe collect decls
    

collect :: Declaration -> Maybe Instance
collect (Instance inst _) = Just inst
collect _ = Nothing
