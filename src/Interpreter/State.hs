module Interpreter.State where


import Control.Monad.State ( State )

import Interpreter.Store ( Store )
import Interpreter.Error ( Evaluation'Error )


type Machine'State a = State Store (Either Evaluation'Error a)
