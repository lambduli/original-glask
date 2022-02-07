module REPL.Load where


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
import Compiler.Analysis.TypeSystem.InferenceEnv

import Compiler.Syntax.Term
import Compiler.Syntax
import Compiler.Syntax.HasKind

import Compiler.Analysis.Semantic.SemanticError

import Compiler.Analysis.TypeSystem.Program
import Compiler.Analysis.TypeSystem.Type.Infer.Program
import Compiler.Analysis.TypeSystem.Binding
import Compiler.Analysis.TypeSystem.Utils.Infer (close'over)
import Compiler.Analysis.TypeSystem.Infer

import Compiler.Analysis.TypeSystem.Solver.Substitution
import Compiler.Analysis.TypeSystem.Solver.Substitutable

import REPL.Repl
import REPL.Analyses
import REPL.ReadExpr


load :: String -> IO ()
load file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  let term'decls = parse contents
  let (trans'env, counter) = build'trans'env term'decls

  case do'semantic'analysis term'decls trans'env of
    Left sem'err -> do
      print sem'err
      return ()

    Right () -> do
      case translate term'decls counter trans'env of
        Left sem'err -> do
          print sem'err
          return ()

        Right declarations -> do
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

          putStrLn "Program:"
          print program

          -- (Type'Env, [Constraint Kind])
          case run'infer infer'env (infer'program program) of
            Left err -> do
              print err
            Right (t'env, k'constrs) -> do
              -- putStrLn "Inference done. ... Maybe ..."


              -- putStrLn "Type Environment:"
              -- print t'env

              -- TODO: I also need to do the Kind inference, probably even before type inference
              -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
              -- for now - I can just infer them together I think
              -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs
              putStrLn "Successfully loaded the prelude."
              
              -- putStrLn "Class Environment:"
              -- print class'env

              -- putStrLn "All Declarations:"
              -- putStrLn $ intercalate "\n" $ map show declarations

              repl (program, infer'env, class'env, trans'env, counter)


parse :: String -> [Term'Decl]
parse = parse'module



