module Interpreter.Evaluate where


import Compiler.Syntax.Name ( Name )

import Interpreter.Address
import Interpreter.Promise
import Interpreter.Environment ( Environment )
import Interpreter.Store
import Interpreter.Value ( Value(..) )
import Interpreter.State ( Machine'State )
import Interpreter.Core ( Core(..), Binding(..), Match(..) )
import Interpreter.Error ( Evaluation'Error(..) )


eval :: Core -> Environment -> Machine'State (Either Evaluation'Error Value)
eval (Var name) env = do
  undefined -- TODO: lookup from the environment and force the promise

eval (Const name) env = do
  undefined -- TODO: lookup from the environment and force the promise

eval (Op name) env = do
  return $! Right $! Operator name

eval (Lit lit) env = do
  return $! Right $! Literal lit

eval (Abs name body) env = do
  undefined

eval (App fn arg) env = do
  undefined

eval (Tuple expressions) env = do
  undefined

eval (Let bindings body) env = do
  undefined

eval (Case motive matches) env = do
  undefined

eval (Intro tag arguments) env = do
  undefined

eval (Error message) env = do
  undefined