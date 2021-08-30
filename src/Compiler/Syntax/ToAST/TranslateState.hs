module Compiler.Syntax.ToAST.TranslateState where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Kind


type Translate'State = Map.Map String Kind
