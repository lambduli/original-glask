module Compiler.Analysis.TypeSystem.Class where


import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Trans.Except (catchE)


import Compiler.Syntax

type Supers = [Name]


type Class = (Supers, [Instance])
