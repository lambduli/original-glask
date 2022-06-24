module REPL.Type where

import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Parser (parse'type)

import Compiler.Syntax.Name
import Compiler.Syntax ( Type, Kind )
import Compiler.Syntax.ToAST.TranslateState ( Translate'State )
import Compiler.Syntax.ToAST.TranslateEnv ( Translate'Env(Trans'Env) )
import Compiler.Syntax.ToAST ( translate )

import Compiler.Analysis.Syntactic.Types

import Compiler.Analysis.Semantic.SemanticError ( Semantic'Error )

import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, overloaded, instances) )
import Compiler.TypeSystem.Infer ( run'infer, get'constraints, Kind'Check )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'type )
import Compiler.TypeSystem.InferenceState ( Infer'State(..) )
import Compiler.TypeSystem.InferenceState as I'State

import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )

import Compiler.TypeSystem.Error ( Error )
import Compiler.Counter (State(get'counter), Counter)


read'type :: String -> Translate'Env -> Translate'State -> Either Semantic'Error (Type, Translate'State)
read'type input t'env t'state = do
  let term'type = parse'type input

  translate term'type t'state t'env


infer'kind :: Type -> Infer'Env -> Counter -> Either Error (Kind, Counter)
infer'kind type' i'env counter = do
  let k'i'state = Infer'State{ counter = counter, constraints = [], I'State.overloaded = [], I'State.instances = [], I'State.holes = [] }

  (kind, k'i'state') <- run'infer i'env (infer'type' type') k'i'state

  let counter' = get'counter k'i'state'

  return (kind, counter')


infer'type' :: Type -> Kind'Check Kind
infer'type' type' = do
  kind <- infer'type type'
  constraints <- get'constraints

  case run'solve constraints :: Either Error (Subst Name Kind) of
    Left err -> throwError err
    Right subst -> do
      let kind' = apply subst kind

      return kind'
