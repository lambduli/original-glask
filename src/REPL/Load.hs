module REPL.Load where


import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra


import Compiler.Counter (Counter)

import Compiler.Parser

import Compiler.Syntax
import Compiler.Syntax.ToAST
import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Syntax.ToAST.Translate
import Compiler.Syntax.Term
import Compiler.Syntax.HasKind
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

import qualified Compiler.Analysis.Semantic.ClassEnv as Class'Env
import qualified Compiler.Analysis.Semantic.Data as Data

import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv
import Compiler.Analysis.Syntactic.Types

import Compiler.Analysis.Semantic.SemanticError

import Compiler.TypeSystem.Error

import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Type.Infer.Program
import Compiler.TypeSystem.Binding
import Compiler.TypeSystem.Utils.Infer
import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..), init't'env )
import Compiler.TypeSystem.ClassEnv ( Class'Env )

import Compiler.TypeSystem.Solver.Substitution ( Subst(Sub) )
import Compiler.TypeSystem.Solver.Substitutable

import REPL.Repl
import REPL.Analyses
import REPL.Expression
import REPL.Program


load :: String -> Counter -> IO ()
load file'name counter = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> do
      putStrLn $ "Semantic Error: " ++ show sem'err

    Right (decls, trans'env, counter') -> do
      -- putStrLn $ "..................     Prave jsem nacetl deklarace  `load'declarations`   a Counter je " ++ show counter

      -- let Program{ bind'sections = bs, methods = _, method'annotations = m'ans, data'declarations = ds } = make'program decls trans'env counter
      -- putStrLn ""
      -- putStrLn "::::::::::"
      -- putStrLn $ "methods:  " ++ show m'ans
      -- putStrLn ""
      -- putStrLn $ "data'declarations:  " ++ show ds
      -- putStrLn "::::::::::"
      -- putStrLn ""
      -- putStrLn $ "bindings:  " ++ show bs



      -- putStrLn $ "\n\n load declarations    trans'env   " ++ show trans'env
      case process'declarations decls trans'env counter' of
        Left err -> do
          putStrLn $ "Error: " ++ show err
        Right (program, infer'env, trans'env, counter'') -> do
          putStrLn "Successfully loaded the prelude."
          putStrLn ""
          -- let Program{ bind'sections = bs, methods = ms, method'annotations = m'ans, data'declarations = ds } = program
          -- putStrLn $ "Program:\n" ++ show program
          -- putStrLn $ "\nbind'sections:  " ++ show bs
          -- putStrLn $ "\nmethods:  " ++ show ms
          -- putStrLn $ "\nmethod'annotations:  " ++ show m'ans
          -- putStrLn $ "\ndata'declarations:   " ++ show ds

          let k'e = kind'env infer'env

          -- putStrLn $ "\n\n\n type env:  " ++ show (type'env infer'env)
          -- putStrLn $ "Kind Env:  " ++ show k'e
      
          -- putStrLn "Class Environment:"
          -- print class'env

          -- putStrLn "All Declarations:"
          -- putStrLn $ intercalate "\n" $ map show declarations

          repl (program, infer'env, trans'env{ kind'context = k'e `Map.union` (kind'context trans'env)}, counter'')


load'declarations :: String -> Counter -> Either Semantic'Error ([Declaration], TE.Translate'Env, Counter)
load'declarations source counter = do
  let term'decls = parse'module source
  let (trans'env, counter') = build'trans'env term'decls counter

  do'semantic'analysis term'decls trans'env

  (declarations, counter'') <- translate term'decls counter' trans'env

  return (declarations, trans'env, counter'')


make'program :: [Declaration] -> TE.Translate'Env -> Counter -> Program
make'program declarations trans'env counter =
  -- TODO: now when I have the list of Declarations in AST form
  -- I need to call inference
  -- for the inference I am going to need to build things like class environment and instance environment
  let class'env = Class'Env.extract declarations


  -- TODO: I need to extract `Type Assumptions` about all data constructors in the list of Declarations
      constr'assumptions = Data.extract declarations


  -- TODO: I need to inclide all bindings in all type classes and instances into the program too
  -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
  -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
  -- then I "just" do the inference
      program :: Program
      program = to'program declarations

      -- m'anns = method'annotations program

  in program


process'declarations :: [Declaration] -> TE.Translate'Env -> Counter -> Either Error (Program, Infer'Env, TE.Translate'Env, Counter)
process'declarations declarations trans'env counter = do
  -- TODO: now when I have the list of Declarations in AST form
  -- I need to call inference
  -- for the inference I am going to need to build things like class environment and instance environment
  let class'env = Class'Env.extract declarations


  -- TODO: I need to extract `Type Assumptions` about all data constructors in the list of Declarations
  let constr'assumptions = Data.extract declarations


  -- TODO: I need to inclide all bindings in all type classes and instances into the program too
  -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
  -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
  -- then I "just" do the inference
  let program :: Program
      program = to'program declarations
      m'anns = method'annotations program
      type'env = init't'env `Map.union` (Map.fromList constr'assumptions) `Map.union` (Map.fromList m'anns)

  let TE.Trans'Env{ TE.kind'context = k'env, TE.classes = class'ctxt } = trans'env

  let infer'env :: Infer'Env
      infer'env = Infer'Env { kind'env = k'env
                            , type'env = type'env
                            , class'env =  class'env
                            , constraint'env = class'ctxt
                            , kind'substitution = Sub Map.empty }

  -- (Type'Env, [Constraint Kind])
  -- (t'env, k'constr) <- run'infer infer'env (infer'program program)

  (t'env', k'env', c'env, cnt, cl'env) <- infer'whole'program program infer'env counter


  -- TODO: I also need to do the Kind inference, probably even before type inference
  -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
  -- for now - I can just infer them together I think
  -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs

  -- NOTE:  I need to merge the alread-known with the newly-inferred
  --        In essence - built-in typing context + method types merging with inferred Implicits + checked Explicits
  -- let type'env' = t'env `Map.union` type'env
  --        I no longer need to do this. I made the `infer'whole'program` apply the kind substitution to the both "base type environment" and the "inferred env from the assumptions"
  --        and union them and return it

  return (program, infer'env{ type'env = t'env', kind'env = k'env', constraint'env = c'env, class'env = cl'env }, trans'env, cnt)
