module Compiler.TypeSystem.Infer (Infer, Type'Check, Kind'Check, run'infer, add'constraints, get'constraints) where


import qualified Data.Map.Strict as Map

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State ( MonadState(get, put), gets, evalStateT, StateT )
import Control.Monad.Except ( Except, runExcept )


import {-# SOURCE #-} Compiler.Syntax.Expression ( Expression )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type )
import Compiler.Syntax.Kind ( Kind )

import Compiler.TypeSystem.Error ( Error )

import Compiler.TypeSystem.InferenceEnv ( Infer'Env )
import Compiler.TypeSystem.InferenceState ( Infer'State (constraints, Infer'State) )
import Compiler.TypeSystem.Constraint ( Constraint )


-- | Inference Monad
type Infer a b
  = ReaderT
      Infer'Env
      (StateT
        (Infer'State b)
        (Except
          Error))
      a


type Type'Check a = Infer a Type


type Kind'Check a = Infer a Kind


run'infer :: Infer'Env -> Infer a b -> Infer'State b -> Either Error (a, Infer'State b)
run'infer env m i'state = runExcept $ evalStateT (runReaderT (run'infer' m) env) i'state


run'infer' :: Infer a b -> Infer (a, Infer'State b) b
run'infer' m = do
  a <- m
  inf'state <- get
  return (a, inf'state)


add'constraints :: [Constraint b] -> Infer () b
add'constraints constrs = do
  i'state <- get
  let current'constrs = constraints i'state
      new'state       = i'state{ constraints = current'constrs ++ constrs }
  put new'state


get'constraints :: Infer [Constraint b] b
get'constraints = do gets constraints
