module Interpreter.Environment where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name ( Name )

import {-# SOURCE #-} Interpreter.Promise ( Promise )


type Environment = Map.Map Name Promise
