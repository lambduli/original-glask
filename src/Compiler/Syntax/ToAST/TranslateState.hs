module Compiler.Syntax.ToAST.TranslateState where


import qualified Data.Map.Strict as Map


import Compiler.Counter ( Counter, State(..) )


type Translate'State = Counter
