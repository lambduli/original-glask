module Main where

import System.IO

import Compiler.Parser.Parser (parse'module)

import Compiler.Syntax.ToAST.Translate
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles
import qualified Compiler.Analysis.Semantic.Synonym.FullyApplied as Applied

import qualified Compiler.Analysis.Syntactic.FixityAnalysis as Fixity
import qualified Compiler.Analysis.Syntactic.ConstructorAnalysis as Constructors
import qualified Compiler.Analysis.Syntactic.SynonymAnalysis as Synonyms

import Compiler.Syntax.ToAST

import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv
import Compiler.Analysis.TypeSystem.InferenceEnv

import Compiler.Syntax.Term
import Compiler.Syntax

import Compiler.Analysis.Semantic.SemanticError


main :: IO ()
main = do
  putStrLn "Glamorous Glask REPL."
  putStrLn ""
  load "prelude.glask"


load :: String -> IO ()
load file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  let term'decls = parse contents
  let trans'env = build'trans'env term'decls
  
  case do'semantic'analysis term'decls trans'env of
    Left sem'err -> do
      print sem'err
      return ()

    Right () -> do
      case translate'to'ast term'decls trans'env of
        Left sem'err -> do
          print sem'err
          return ()

        Right declarations -> do
          -- TODO: now when I have the list of Declarations in AST form
          -- I need to call inference
          -- for the inference I am going to need to build things like class environment and instance environment
          -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
          -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
          -- then I "just" do the inference

          -- TODO: I also need to do the Kind inference, probably even before type inference
          -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
          -- for now - I can just infer them together I think
          -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs
          putStrLn "Successfully read the file."
          print declarations
          return ()


parse :: String -> [Term'Decl]
parse = parse'module


build'trans'env :: [Term'Decl] -> TE.Translate'Env
build'trans'env declarations = do
  -- TODO: now I need to run all the analyses, use them to translate to ast
  -- build the environment for the to'ast translation
  -- also to initialize the translation with some initial state, which should be already prepared somewhere
  let fixities :: Fixity'Env
      fixities = Fixity.analyze declarations

  let -- (constructors, fields) :: (Constr'Env, FIeld'Env)
      (constructors, fields) = Constructors.analyze declarations

  let kind'context :: Kind'Env
      kind'context = init'k'env
      -- TODO: this should not be empty, it needs to contain kinds of all known type constructors
      -- that means primitive types like Int, Char, Tuple, Unit, List, Bool, (->)
      -- am I missing something?
      -- YES! It also needs to contain assignments for all user declared types
      -- TODO: add all user defined types declared with `data` keyword
      -- Think about type synonyms, there are at least two ways to go about them
      -- I can ignore them in this step, because they will be fully erased in the following step
      -- so (for instance) the result of `to'ast` will not contain any type synonym declarations and the code will not too
      -- OR I keep the synonyms bit longer
      -- but honestly, I dont' really see the value in that
      -- since I already do Fully Applied and Cycle analyses on the Term level
      -- I don't think it's necessary to translate synonyms to AST too

  let synonyms :: Synonym'Env
      synonyms = Synonyms.analyze declarations

  TE.Trans'Env{ TE.fixities = fixities, TE.constructors = constructors, TE.fields = fields, TE.kind'context = kind'context, TE.synonyms = synonyms }


-- TODO: implement checking that every declaration which needs to be unique is in fact unique
--        like no types declared multiple times, same with functions and synonyms, ...
do'semantic'analysis :: [Term'Decl] -> TE.Translate'Env -> Either Semantic'Error ()
do'semantic'analysis declarations TE.Trans'Env{ TE.synonyms = synonyms } = do
  -- TODO: I can start with semantic analysis
  let errors = Applied.analyze synonyms declarations
  raise errors -- only of there are any

  let errors = Cycles.analyze synonyms declarations
  raise errors


raise :: [Semantic'Error] -> Either Semantic'Error ()
raise [] = Right ()
raise errors = Left $ Many'Errors errors


-- TODO: this function also needs to merge all the binding groups of the same name together
translate'to'ast :: [Term'Decl] -> TE.Translate'Env -> Either Semantic'Error [Declaration]
translate'to'ast declarations trans'env
  = -- TODO: now to run the translation using some sort of run'X function
  run'translate trans'env (to'ast declarations :: Translate [Declaration])
