module Compiler.Analysis.Semantic.Synonym.Cycles where


import Compiler.Syntax (Declaration)
import Compiler.Analysis.Semantic.SemanticError (Semantic'Error)


analyze :: [Declaration] -> Maybe [Semantic'Error]
analyze = undefined