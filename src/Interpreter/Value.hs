module Interpreter.Value where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal ( Literal )


import {-# SOURCE #-} Interpreter.Core ( Core )
import Interpreter.Promise ( Promise )
import Interpreter.Environment ( Environment )


data Value
  = Literal Literal
  | Operator Name
  | Closure Name Core Environment
  | Data String [Promise] -- Name of the Constr and list of arguments
  deriving (Show)
