module Interpreter.Promise where


import Interpreter.Address ( Address )


newtype Promise = Promise Address
  deriving (Show, Eq)
