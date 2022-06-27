module Interpreter.Promise where


import Compiler.Syntax.Expression ( Expression )

import Interpreter.Environment ( Environment )
import Interpreter.Address ( Address )


newtype Promise = Promise Address
  deriving (Show, Eq)
