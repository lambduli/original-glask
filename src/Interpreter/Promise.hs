module Interpreter.Promise where


import Compiler.Syntax.Expression ( Expression )

import Interpreter.Environment ( Environment )
import Interpreter.Address ( Address )


data Promise = Promise Expression Environment Address