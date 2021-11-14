module Compiler.Syntax.ToAST.TranslateEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name

import Compiler.Analysis.Syntactic.ConstrEnv
import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv
import Compiler.Analysis.Syntactic.KindContext


data Translate'Env = Trans'Env
  { fixities :: Fixity'Env
  , constructors :: Constr'Env
  , fields :: Field'Env
  , kind'context :: Kind'Context
  , synonyms :: Synonym'Env }
