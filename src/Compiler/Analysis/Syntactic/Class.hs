module Compiler.Analysis.Syntactic.Class where


import Data.Maybe (mapMaybe)


import Compiler.Syntax.Declaration ( Data, Class, Declaration(Class'Decl) )


extract :: [Declaration] -> [Class]
extract decls = mapMaybe collect decls
    

collect :: Declaration -> Maybe Class
collect (Class'Decl class') = Just class'
collect _ = Nothing
