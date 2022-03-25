{-# LANGUAGE MultiParamTypeClasses  #-}

module REPL.Expression where


import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra
import Control.Monad.Identity 
import Control.Monad.Except


import Compiler.Parser ( parse'expr )

import Compiler.Counter

import Compiler.Syntax.ToAST.Translate
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles
import qualified Compiler.Analysis.Semantic.Synonym.FullyApplied as Applied

import qualified Compiler.Analysis.Syntactic.Fixity as Fixity
import qualified Compiler.Analysis.Syntactic.Constructors as Constructors
import qualified Compiler.Analysis.Syntactic.Synonyms as Synonyms
import qualified Compiler.Analysis.Syntactic.Types as Types

import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Syntactic.Data as Data

-- import qualified Compiler.Analysis.Semantic.Dependency.BindingAnalysis as Dependencies
import qualified Compiler.Analysis.Semantic.ClassEnv as Classes'Env

import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv

import Compiler.Syntax.ToAST
import Compiler.Syntax.Term
import Compiler.Syntax
import Compiler.Syntax.HasKind
import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Syntax.ToAST.TranslateState

import Compiler.Analysis.Semantic.SemanticError

import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Type.Infer.Program
import Compiler.TypeSystem.Binding
import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Utils.Infer
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.Error
import Compiler.TypeSystem.Type.Infer.Expression
import Compiler.TypeSystem.Solver
import Compiler.TypeSystem.Utils.Class
import Compiler.TypeSystem.Solver.Composable
import Compiler.TypeSystem.InferenceState ( Infer'State(..) )
import qualified Compiler.TypeSystem.InferenceState as I'State

import Compiler.TypeSystem.Expected ( Expected(Infer) )
import Compiler.TypeSystem.Actual ( Actual(Inferred) )
import Compiler.Counter (State(get'counter))
import Compiler.TypeSystem.Infer (get'constraints, Type'Check)
import Compiler.Syntax (Expression, Predicate)
import Compiler.TypeSystem.Constraint (Constraint)


import Debug.Trace


read'expr :: String -> Translate'Env -> Translate'State -> Either Semantic'Error (Expression, Translate'State)
read'expr input trans'env trans'state = do
  let term'expr = parse'expr input

  -- checking kind of analysis
  -- NOTE:  This would normally check that all synonyms are fully applied and their definitions are not cyclic in any way.
  --        But the thing is - I am currently not even expanding the type synonyms, so doing this analysis is kinda pointless.
  --        I can just pretend like I don't support Type Synonyms for little bit longer.
  --        TODO: Then this should be fixed correctly.
  -- do'semantic'analysis term'decls trans'env

  -- translating to the AST form
  translate term'expr trans'state trans'env

  -- let class'env = Classes.extract declarations'Env

  -- let program :: Program
  --     program = to'program declarations
  --     m'anns = method'annotations program
  --     type'env = Map.union init't'env $ Map.fromList $ map (second close'over) m'anns

  -- let TE.Trans'Env{ TE.kind'context = k'env } = trans'env

  -- let infer'env :: Infer'Env
  --     infer'env = Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }


-- TODO: move this function into a TypeSystem module (and rename it probably)
infer'type :: Expression -> Infer'Env -> Counter -> Either Error (Sigma'Type, Counter)
infer'type expr i'env counter = do
  -- ([Predicate], Type, [Constraint Type], [Constraint Kind])
  let t'i'state = Infer'State{ I'State.counter = counter, constraints = [] }
  ((preds, actual, cs't), i'state') <- run'infer i'env (infer'expr' expr Infer) t'i'state

  -- TODO: refactor later (get rid of the pattern matching, possibly by calling function which actually returns type instead of Actual)
  type' <- case actual of
              Inferred t  -> Right t
              _           -> Left $ Unexpected "Internal Error - infer'type in the REPL" 

  subst <- run'solve cs't  :: Either Error (Subst M'V Type)

  let Infer'Env{ type'env = t'env, class'env = c'env } = i'env
  
  rs <- runIdentity $ runExceptT $ reduce c'env (apply subst preds) :: Either Error [Predicate]

  -- NOTE:  Maybe I don't need to do any defaulting when inferring just a single Expression and inside the REPL?
  -- TODO:  I need to find out what exactly would that defaulting substitution do to the Expression Type.
  --        I am pretty sure I saw it also do the monomorphisation restriction. I might want to disable that one in the REPL too.
  -- s' <- runIdentity $ runExceptT $ default'subst c'env [] rs :: Either Error (Subst T'V Type)
  let s' = Sub Map.empty

  subst' <- runIdentity $ runExceptT (s' `merge` subst)

  let scheme = close'over $ apply subst' rs :=> apply subst' type'

  let counter' = get'counter i'state'

  -- let message = "{{ tracing infer'type in REPL }}"
  --               ++ "\n | scheme: " ++ show scheme
  --               ++ "\n | before close'over: " ++ show (apply subst' rs :=> apply subst' type')
  --               ++ "\n | rs: " ++ show rs
  --               ++ "\n | preds: " ++ show preds
  --               ++ "\n | preds substituted: " ++ show (apply subst preds)
  --               ++ "\n | type': " ++ show type'
  --               ++ "\n | constraints: " ++ show cs't
  --               ++ "\n | subst': " ++ show subst'
  --               ++ "\n | subst: " ++ show subst
  --     oo = trace message scheme

  return (scheme, counter')


infer'expr' :: Expression -> Expected Type -> Type'Check ([Predicate], Actual Type, [Constraint Type])
infer'expr' expr expected = do
  (preds, actual) <- infer'expr expr expected

  constraints <- get'constraints

  return (preds, actual, constraints)
