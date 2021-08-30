module Compiler.Syntax.ToAST.Translate where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Syntax.ToAST.TranslateState
import Compiler.Syntax.ToAST.SemanticError



type Translate a
  = ReaderT
      Translate'Env         -- | info about fixities of various operators and functions?
      (StateT           
        Translate'State     -- | association of a Type Variable name and a Kind
        (Except
          Semantic'Error))  -- | offenses against Semantic Rules
      a                     -- | Result
