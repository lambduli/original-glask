module Interpreter.Store where


import qualified Data.Map.Strict as Map

import Interpreter.Address ( Address )
import Interpreter.Value ( Value )


type Store = Map.Map Address Value
