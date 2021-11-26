module Main where

import System.IO
import Data.List
import qualified Data.Map.Strict as Map


import Compiler.Parser.Parser (parse'module)

import Compiler.Syntax.ToAST.Translate
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles
import qualified Compiler.Analysis.Semantic.Synonym.FullyApplied as Applied

import qualified Compiler.Analysis.Syntactic.FixityAnalysis as Fixity
import qualified Compiler.Analysis.Syntactic.ConstructorAnalysis as Constructors
import qualified Compiler.Analysis.Syntactic.SynonymAnalysis as Synonyms
import qualified Compiler.Analysis.Syntactic.Types as Types

import qualified Compiler.Analysis.Semantic.Class as Classes

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
  let (trans'env, counter) = build'trans'env term'decls
  
  case do'semantic'analysis term'decls trans'env of
    Left sem'err -> do
      print sem'err
      return ()

    Right () -> do
      case translate'to'ast term'decls counter trans'env of
        Left sem'err -> do
          print sem'err
          return ()

        Right declarations -> do
          -- TODO: now when I have the list of Declarations in AST form
          -- I need to call inference
          -- for the inference I am going to need to build things like class environment and instance environment
          let class'env = Classes.analyze declarations


          -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
          -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
          -- then I "just" do the inference

          -- TODO: I also need to do the Kind inference, probably even before type inference
          -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
          -- for now - I can just infer them together I think
          -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs
          putStrLn "Successfully read the file."
          
          putStrLn "-----------------------"
          
          putStrLn "Class Environment:"
          print class'env
          
          putStrLn "All Declarations:"
          putStrLn $ intercalate "\n" $ map show declarations
          return ()


parse :: String -> [Term'Decl]
parse = parse'module


-- NOTE: ragarding the Int part of the result -- follow the trail of it (out of this function) and read the comments if you don't know why it's there
--        In the README there's a comment/idea regarding a more proper implementation.
build'trans'env :: [Term'Decl] -> (TE.Translate'Env, Int)
build'trans'env declarations = do
  -- TODO: now I need to run all the analyses, use them to translate to ast
  -- build the environment for the to'ast translation
  -- also to initialize the translation with some initial state, which should be already prepared somewhere
  let fixities :: Fixity'Env
      fixities = Fixity.analyze declarations

  let -- (constructors, fields) :: (Constr'Env, FIeld'Env)
      (constructors, fields) = Constructors.analyze declarations


  let user'kind'context :: Kind'Env
      (user'kind'context, Types.Counter{ Types.count = count }) = Types.analyze declarations
  -- TODO: now the `count` should be used to initialize the counter in the Translate'State

  let kind'context :: Kind'Env
      kind'context = init'k'env `Map.union` user'kind'context
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

  (TE.Trans'Env{ TE.fixities = fixities, TE.constructors = constructors, TE.fields = fields, TE.kind'context = kind'context, TE.synonyms = synonyms }, count)


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


-- NOTE and TODO:
-- The second argument counter ::Int is just a temporary solution to the problem I have ran into.
-- When I want to build a Trans'Env I need to collect all user defined type constructors and assign them a Kind build from fresh Kind Variables
-- BUT, to do that, I need to be inside a State monad. Because I need to be able to increment the counter.
-- On a first glance, it would seem that I could be inside a Translate monad stack, BUT that is not the case, since I am, at that moment, building
-- that very thing (collecting all the parts of the Trans'Env)
-- So I am left with the other choice - I say I am in the context of the State monad which only contains the Counter
-- that means, however, that I will need to pick the final value of the counter AFTER I collect all the type constructors and assign them a Kind
-- and use that as a intial value for the counter in the Translate'State
-- for that exact reason, this parameter needs to be passed through few levels and be used here

-- TODO: this function also needs to merge all the binding groups of the same name together
translate'to'ast :: [Term'Decl] -> Int -> TE.Translate'Env -> Either Semantic'Error [Declaration]
translate'to'ast declarations counter trans'env
  = -- TODO: now to run the translation using some sort of run'X function
  run'translate counter trans'env (to'ast declarations :: Translate [Declaration])
