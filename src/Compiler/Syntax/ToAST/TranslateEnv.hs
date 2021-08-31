module Compiler.Syntax.ToAST.TranslateEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Kind

import Compiler.Analysis.Syntactic.ConstrEnv
import Compiler.Analysis.Syntactic.FixityEnv


data Translate'Env = Trans'Env
  { fixities :: Fixity'Env
  , constructors :: Constr'Env
  , typing'scope :: Map.Map String Kind }
