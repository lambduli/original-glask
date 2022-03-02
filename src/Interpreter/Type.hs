module Interpreter.Type where


import Compiler.Parser (parse'type)

import Compiler.Syntax.Name
import Compiler.Syntax ( Type, Kind )
import Compiler.Syntax.ToAST.TranslateState ( Translate'State )
import Compiler.Syntax.ToAST.TranslateEnv ( Translate'Env(Trans'Env) )
import Compiler.Syntax.ToAST ( translate )

import Compiler.Analysis.Syntactic.Types

import Compiler.Analysis.Semantic.SemanticError ( Semantic'Error )

import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env) )
import Compiler.TypeSystem.Infer ( run'infer )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'type )
import Compiler.TypeSystem.InferenceState ( Infer'State )

import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )

import Compiler.TypeSystem.Error ( Error )


read'type :: String -> Translate'Env -> Translate'State -> Either Semantic'Error (Type, Translate'State)
read'type input t'env t'state = do
  let term'type = parse'type input

  translate term'type t'state t'env


infer'kind :: Type -> Infer'Env -> Infer'State -> Either Error (Kind, Infer'State)
infer'kind type' i'env i'state = do
  ((kind, k'cs), i'state') <- run'infer i'env (infer'type type') i'state

  subst <- run'solve k'cs :: Either Error (Subst Name Kind)

  let kind' = apply subst kind

  return (kind', i'state')