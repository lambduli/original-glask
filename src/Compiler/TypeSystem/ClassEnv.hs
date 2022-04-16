module Compiler.TypeSystem.ClassEnv where


import qualified Data.Map.Strict as Map

import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type )

import Compiler.TypeSystem.Class ( Class )


data Class'Env = Class'Env  { classes :: Map.Map Name Class
                            , defaults :: [Type] }
                  deriving (Show)
