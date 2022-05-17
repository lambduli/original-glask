module Interpreter.Store where


import qualified Data.Map.Strict as Map

import Interpreter.Address ( Address )
import Interpreter.Value ( Value )
import Interpreter.Core ( Core )
import Interpreter.Environment ( Environment )


type Store = Map.Map Address Promise'Content


type Promise'Content = Either (Core, Environment) Value
