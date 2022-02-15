module Compiler.Syntax.ToAST.TranslateState where


import qualified Data.Map.Strict as Map


import Compiler.Counter


type Translate'State = Counter



-- | Translation State
-- | count :: Int is used for creating a fresh and unique Kind Variable
-- newtype Translate'State = Translate'State{ count :: Int }


-- -- | initial Inference State
-- init'translate'state :: Translate'State
-- init'translate'state = Translate'State{ count = 0 }
