module Compiler.Syntax.ToAST.TranslateEnv where


import Compiler.Syntax.Name
import Compiler.Syntax.Fixity
import Compiler.Syntax.Type

import Compiler.Analysis.ConstrEnv


data Translate'Env = Trans'Env
  { fixities :: (Fixity, Int, Name)
  , constructors :: Constr'Env }
