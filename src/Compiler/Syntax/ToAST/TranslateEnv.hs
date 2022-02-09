module Compiler.Syntax.ToAST.TranslateEnv where


import qualified Data.Map.Strict as Map


import Compiler.Analysis.Syntactic.ConstrEnv ( Constr'Env )
import Compiler.Analysis.Syntactic.FixityEnv ( Fixity'Env )
import Compiler.Analysis.Syntactic.FieldEnv ( Field'Env )
import Compiler.Analysis.Syntactic.SynonymEnv ( Synonym'Env )

import Compiler.TypeSystem.InferenceEnv ( Kind'Env )


data Translate'Env = Trans'Env
  { fixities :: Fixity'Env
  , constructors :: Constr'Env
  , fields :: Field'Env
  , kind'context :: Kind'Env
  , synonyms :: Synonym'Env }
