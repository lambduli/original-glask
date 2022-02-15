module Compiler.Analysis.Syntactic.Data where


import Data.Maybe (mapMaybe)


import Compiler.Syntax.Declaration ( Data, Declaration(Data'Decl) )


extract :: [Declaration] -> [Data]
extract decls = mapMaybe collect decls
    

collect :: Declaration -> Maybe Data
collect (Data'Decl data') = Just data'
collect _ = Nothing
