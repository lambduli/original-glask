module Compiler.Syntax.ToAST.Translate (Translate, run'translate) where


import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except


import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Syntax.ToAST.TranslateState
import Compiler.Analysis.Semantic.SemanticError



type Translate a
  = ReaderT
      Translate'Env         -- | info about fixities of various operators and functions?
      (StateT           
        Translate'State     -- | association of a Type Variable name and a Kind
        (Except
          Semantic'Error))  -- | offenses against Semantic Rules
      a                     -- | Result


-- NOTE and TODO:
-- The first argument counter ::Int is just a temporary solution to the problem I have ran into.
-- When I want to build a Trans'Env I need to collect all user defined type constructors and assign them a Kind build from fresh Kind Variables
-- BUT, to do that, I need to be inside a State monad. Because I need to be able to increment the counter.
-- On a first glance, it would seem that I could be inside a Translate monad stack, BUT that is not the case, since I am, at that moment, building
-- that very thing (collecting all the parts of the Trans'Env)
-- So I am left with the other choice - I say I am in the context of the State monad which only contains the Counter
-- that means, however, that I will need to pick the final value of the counter AFTER I collect all the type constructors and assign them a Kind
-- and use that as a intial value for the counter in the Translate'State
-- for that exact reason, this parameter needs to be passed through few levels and be used here
run'translate :: Translate'Env -> Translate a -> Translate'State -> Either Semantic'Error (a, Translate'State)
run'translate env m tr'state = runExcept $ evalStateT (runReaderT (run'translate' m) env) tr'state -- Translate'State{ count = counter } -- init'translate'state


run'translate' :: Translate a -> Translate (a, Translate'State)
run'translate' m = do
  a <- m
  tr'state <- get
  return (a, tr'state)