module Interpreter.State where


import Control.Monad.State ( State )

import Interpreter.Store ( Store )


type Machine'State a = State Store a
