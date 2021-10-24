module Compiler.Analysis.Syntactic.ConstrEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import Compiler.Syntax.Type


{-  Associates a Name and a Constr'Info
    Since each constructor and Record-Field-Name has to have a unique identifier
    this Map holds the globally-scoped information.
-}
type Constr'Env = Map.Map Name Constr'Info


{- NOTE:
  Orinary constructors currently don't need any additional information.
  Constructors defined using record syntax need to hold the information
  about each field in some specific order.
  The order is the same as in which the fields were given. That way record syntax and ordinary syntax
  can be used interchangeably even by the user.
  The positional order corresponds to the order in which the fields were given. -}
data Constr'Info
  = Constr
  | Record { fields :: [Name] }
