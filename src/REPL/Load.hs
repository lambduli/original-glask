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
-- import Compiler.Syntax.ToAST.Translate
-- import Compiler.Syntax.Term
-- import Compiler.Syntax.HasKind
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles
import qualified Compiler.Analysis.Semantic.Synonym.FullyApplied as Applied

import qualified Compiler.Analysis.Syntactic.Fixity as Fixity
import qualified Compiler.Analysis.Syntactic.Constructors as Constructors
import qualified Compiler.Analysis.Syntactic.Synonyms as Synonyms
import qualified Compiler.Analysis.Syntactic.Types as Types
import qualified Compiler.Analysis.Syntactic.Instance as Instances

import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings

import qualified Compiler.Analysis.Semantic.ClassEnv as Class'Env
import qualified Compiler.Analysis.Semantic.Data as Data

-- import Compiler.Analysis.Syntactic.FixityEnv
-- import Compiler.Analysis.Syntactic.FieldEnv
-- import Compiler.Analysis.Syntactic.SynonymEnv
-- import Compiler.Analysis.Syntactic.Types

import Compiler.Analysis.Semantic.SemanticError

import Compiler.TypeSystem.Error

import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Type.Infer.Program
-- import Compiler.TypeSystem.Binding
-- import Compiler.TypeSystem.Utils.Infer
-- import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..), init't'env )
import Compiler.TypeSystem.ClassEnv ( Class'Env )

import Compiler.TypeSystem.Solver.Substitution ( Subst(Sub) )
-- import Compiler.TypeSystem.Solver.Substitutable

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
          putStrLn $ "Program:\n" ++ show program ++ "\n\n\n"
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
  -- for the inference I am going to need to build things like class environment and instance environment
  let class'env = Class'Env.extract declarations


  -- TODO: I need to build overloaded
  -- overloaded contains three kinds of associations
  -- overloads - overloaded functions/constants
    -- these are two kinds, either implicitly typed or explicitly typed
    -- implicits are now taken care of in the function which infers groups
    -- explicits could be registered when they are put into the typing context - done
  -- methods
    -- I can also take care of it in one of that infer'types function - done
  -- recursive
    -- those are kind strange, but should be taken care of by the infer'impls
    -- at the beginning of it, each mutual binding gets free type variable and association of recursive
    -- the association only lasts for current scope and is later replaced with proper overload (might) if it has a qualified type by the group function
  -- so in fact, here I don't need to do anything


  -- TODO: To build `instance'env`
  -- I can use it to store all the instances there.
  let instances   = Instances.extract declarations
      penta'tuples  = map to'penta'tuple instances
      to'penta'tuple :: Instance -> ((Name, Type), (Name, [Predicate], Predicate))
      to'penta'tuple (context :=> p@(Is'In cl'name type')) =
        let ty'const@(T'Con (T'C ty'name _)) = case type' of
                                                T'Var' t' -> error "illegal syntactically"
                                                T'Meta m'v -> error "illegal syntactically"
                                                T'Con (T'C name _) -> type'
                                                T'Tuple tys -> error "not supporting tuples now"
                                                -- TODO: but later it would be the specific constructor for the tuple, like `(,)` for a pair
                                                T'App ty _ -> get'ty'const ty
                                                T'Forall t's qual -> error "illegal syntactically"
            dictionary'name = "d-" ++ cl'name ++ "-" ++ ty'name
        -- ((Type Constant Name, Type Class Name), (dictionary global variable name, instance context, the whole type))
        in ((cl'name, ty'const), (dictionary'name, context, p))
      get'ty'const :: Type -> Type
      get'ty'const (T'App ty _) = get'ty'const ty
      get'ty'const ty@(T'Con (T'C name _)) = ty
      get'ty'const _ = error "shouldn't have happened"
  -- now I just trim the the pentas to only associate to the name of the global variable
      insts = map (\ (x, (n, _, _)) -> (x, n)) penta'tuples



  -- TODO: I need to extract `Type Assumptions` about all data constructors in the list of Declarations
  let constr'assumptions = Data.extract declarations


  -- TODO: I need to inclide all bindings in all type classes and instances into the program too
  -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
  -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
  -- then I "just" do the inference
  let program :: Program
      program = to'program declarations
      m'anns = method'annotations program
      type'env = init't'env `Map.union` (Map.fromList constr'assumptions) `Map.union` (Map.fromList $ map (\ (name, type', _) -> (name, type')) m'anns) -- this maping is dirty, but I need just method name and its type

  let TE.Trans'Env{ TE.kind'context = k'env, TE.classes = class'ctxt } = trans'env

  let infer'env :: Infer'Env
      infer'env = Infer'Env { kind'env = k'env
                            , type'env = type'env
                            , class'env =  class'env
                            , constraint'env = class'ctxt
                            , kind'substitution = Sub Map.empty
                            , instance'env = insts -- [((Name, Type), Name)] ||| (Name, Type) is the kind of information in the placeholder, like Num Int, or Eq []
                            , overloaded = [] -- [(Name, Overloaded)]
                            , instances = penta'tuples }

  -- (Type'Env, [Constraint Kind])
  -- (t'env, k'constr) <- run'infer infer'env (infer'program program)

  (program', t'env', k'env', c'env, cnt, cl'env) <- infer'whole'program program infer'env counter


  -- TODO: I also need to do the Kind inference, probably even before type inference
  -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
  -- for now - I can just infer them together I think
  -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs

  -- NOTE:  I need to merge the alread-known with the newly-inferred
  --        In essence - built-in typing context + method types merging with inferred Implicits + checked Explicits
  -- let type'env' = t'env `Map.union` type'env
  --        I no longer need to do this. I made the `infer'whole'program` apply the kind substitution to the both "base type environment" and the "inferred env from the assumptions"
  --        and union them and return it

  return (program', infer'env{ type'env = t'env', kind'env = k'env', constraint'env = c'env, class'env = cl'env }, trans'env, cnt)
