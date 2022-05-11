module Interpreter.Value where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal ( Literal )
import Compiler.Syntax.Expression ( Expression )

import Interpreter.Promise ( Promise )
import Interpreter.Environment ( Environment )


data Value
  = Literal Literal
  | Operator Name
  | Closure Name Expression Environment
  | Data String [Promise] -- Name of the Constr and list of arguments
