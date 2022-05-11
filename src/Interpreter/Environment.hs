module Interpreter.Environment where


import qualified Data.Map.Strict as Map

import Interpreter.Address ( Address )
import {-# SOURCE #-} Interpreter.Promise ( Promise )


type Environment = Map.Map Address Promise
