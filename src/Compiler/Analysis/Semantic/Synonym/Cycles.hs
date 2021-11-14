module Compiler.Analysis.Semantic.Synonym.Cycles where


import Compiler.Syntax.Term
import Compiler.Analysis.Semantic.SemanticError (Semantic'Error)
import Compiler.Analysis.Syntactic.SynonymEnv


-- TODO: I can use the dependency analysis from the Data library
-- and if I find any Mutually Recursive Strongly Connected Component --> that's and error
-- I also need to check that some type synonym doesn't depend on itself
analyze :: Synonym'Env -> [Term'Decl] -> [Semantic'Error]
analyze = undefined