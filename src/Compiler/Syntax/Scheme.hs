module Compiler.Syntax.Scheme where

import Compiler.Syntax.Type
import Compiler.Syntax.Qualified


data Scheme
  = ForAll [T'V] (Qualified Type) -- TODO: continue here - refactor according the paper
  deriving (Eq)


instance Show Scheme where
  show (ForAll [] qual'type')
    = show qual'type'
  show (ForAll type'args qual'type')
    = "forall " ++ unwords (map show type'args) ++ " . " ++ show qual'type'


-- NOTE: temporarily I will put it here
quantify :: [T'V] -> Qualified Type -> Scheme
quantify = ForAll
-- now, here's the thing
-- because I keep both names and kinds of the parametrized type variables
-- as opposed to only keeping kinds (as does Jones)
-- I don't really need to do anything about vars

-- similarly, because I don't represent bound type variables with distinct data construct (TGen)
-- I don't really need to do any substitution application on the qualified type

-- now, that begs the question, do I really need to have this function?
-- I think not.