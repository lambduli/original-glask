module Compiler.Syntax.Declaration where

import Data.List


import {-# SOURCE #-} Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Signature
import Compiler.Syntax.Name
import Compiler.Syntax.MatchGroup


data Declaration
  = Binding Match'Group                     -- id x = x
  -- | Annotated String Type Expression        -- id :: a -> a ; id x = x
  -- I think this one actually might not be that usefull
  -- I can use combination of Signature and Binding
  | Signature Signature                     -- id :: a -> a
  | Data'Decl Name [Name] [Constr'Decl]     -- Data type declaration -- name type'params list'of'consturctors'with'params
  | Type'Alias Name Type                    -- type String = List Char


instance Show Declaration where
  show (Binding match'group)
    = "TODO: show match'group"
    -- = name ++ " = " ++ show expr
  -- show (Annotated name type' expr)
    -- = name ++ " :: " ++ show type' ++ "\n" ++ name ++ " = " ++ show expr
  show (Signature sig)
    = show sig
  show (Data'Decl name params constrs)
    = "data " ++ name ++ " " ++ unwords params ++ " = " ++ intercalate " | " (map show constrs)
  show (Type'Alias name type')
    = "type " ++ name ++ " = " ++ show type'


data Constr'Decl
  = Con'Decl Name [Type]
  | Con'Record'Decl Name [(Name, Type)]
  deriving (Eq)


instance Show Constr'Decl where
  show (Con'Decl name types)
    = name ++ " " ++ unwords (map show types)
  show (Con'Record'Decl name pairs)
    = name ++ " { " ++ intercalate ", " (map (\ (name, type') -> name ++ " :: " ++ show type') pairs) ++ " }"
