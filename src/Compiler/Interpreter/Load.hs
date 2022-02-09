module Compiler.Interpreter.Load where


import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra


import Compiler.Parser.Parser (parse'module)

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

import qualified Compiler.Analysis.Semantic.DependencyAnalysis as Dependencies
import qualified Compiler.Analysis.Semantic.Class as Classes

import Compiler.Syntax.ToAST

import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv
import Compiler.TypeSystem.InferenceEnv
import Compiler.Analysis.Syntactic.Types

import Compiler.Syntax.Term
import Compiler.Syntax
import Compiler.Syntax.HasKind

import Compiler.Analysis.Semantic.SemanticError

import Compiler.TypeSystem.Error

import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Type.Infer.Program
import Compiler.TypeSystem.Binding
import Compiler.TypeSystem.Utils.Infer (close'over)
import Compiler.TypeSystem.Infer

import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable

import Compiler.Interpreter.Repl
import Compiler.Interpreter.Analyses
import Compiler.Interpreter.ReadExpr


load :: String -> IO ()
load file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents of
    Left sem'err -> do
      putStrLn $ "Semantic Error: " ++ show sem'err

    Right (decls, trans'env, counter) ->
      case process'declarations decls trans'env counter of
        Left err -> do
          putStrLn $ "Error: " ++ show err
        Right (program, infer'env, class'env, trans'env, counter) -> do
          putStrLn "Successfully loaded the prelude."
      
          -- putStrLn "Class Environment:"
          -- print class'env

          -- putStrLn "All Declarations:"
          -- putStrLn $ intercalate "\n" $ map show declarations

          repl (program, infer'env, class'env, trans'env, counter)


parse :: String -> [Term'Decl]
parse = parse'module


load'declarations :: String -> Either Semantic'Error ([Declaration], TE.Translate'Env, Counter)
load'declarations source = do
  let term'decls = parse source
  let (trans'env, counter) = build'trans'env term'decls

  do'semantic'analysis term'decls trans'env

  declarations <- translate term'decls counter trans'env

  return (declarations, trans'env, counter)


process'declarations :: [Declaration] -> TE.Translate'Env -> Counter -> Either Error (Program, Infer'Env, Class'Env, TE.Translate'Env, Counter)
process'declarations declarations trans'env counter = do
  -- TODO: now when I have the list of Declarations in AST form
  -- I need to call inference
  -- for the inference I am going to need to build things like class environment and instance environment
  let class'env = Classes.extract declarations


  -- TODO: I need to inclide all bindings in all type classes and instances into the program too
  -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
  -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
  -- then I "just" do the inference
  let program :: Program
      program = to'program declarations
      m'anns = method'annotations program
      type'env = Map.union init't'env $ Map.fromList $ map (second close'over) m'anns

  let TE.Trans'Env{ TE.kind'context = k'env } = trans'env

  let infer'env :: Infer'Env
      infer'env = Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }

  -- (Type'Env, [Constraint Kind])
  (t'env, k'constr) <- run'infer infer'env (infer'program program)


  -- TODO: I also need to do the Kind inference, probably even before type inference
  -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
  -- for now - I can just infer them together I think
  -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs

  -- NOTE:  I need to merge the alread-known with the newly-inferred
  --        In essence - built-in typing context + method types merging with inferred Implicits + checked Explicits
  let type'env' = type'env `Map.union` t'env

  return (program, infer'env{ type'env = type'env' }, class'env, trans'env, counter)
