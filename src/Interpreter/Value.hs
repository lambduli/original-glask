module Interpreter.Value where


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal ( Literal )


import {-# SOURCE #-} Interpreter.Core ( Core )
import Interpreter.Promise ( Promise )
import Interpreter.Environment ( Environment )
import Data.List (intercalate)


data Value
  = Literal Literal
  | Operator Name
  | Closure Name Core Environment
  | Data String [Promise] -- Name of the Constr and list of arguments
  deriving (Show)

-- instance Show Value where
--   show (Literal literal)
--     = show literal

--   show (Operator name)
--     = name

--   show (Closure param body env)
--     = "(\\ " ++ param ++ " -> " ++ show body ++ ")"

--   show (Data tag promises)
--     = "<" ++ tag ++ " " ++ unwords (map show promises) ++ ">"
