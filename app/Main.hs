module Main where

import System.IO

import qualified Data.Map.Strict as Map

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
import Compiler.Analysis.Syntactic.KindContext

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

  case parse contents of
    Left sem'err -> do
      print sem'err
      return ()

    Right declarations -> do
      putStrLn "Successfully read the file."
      return ()


-- TODO: split this function into two
-- one is only going to parse (maybe just call parse'module)
-- the other one does the semantic analysis
-- and thinking of it - maybe the third one does the translation to the AST
parse :: String -> Either Semantic'Error [Declaration]
parse source = do
  case parse'module source of
    declarations -> do
      -- TODO: now I need to run all the analyses, use them to translate to ast
      -- build the environment for the to'ast translation
      -- also to initialize the translation with some initial state, which should be already prepared somewhere
      let fixities :: Fixity'Env
          fixities = Fixity.analyze declarations

      let -- (constructors, fields) :: (Constr'Env, FIeld'Env)
          (constructors, fields) = Constructors.analyze declarations

      let kind'context :: Kind'Context
          kind'context = init'kind'context
          -- TODO: this should not be empty, it needs to contain kinds of all known type constructors
          -- that means primitive types like Int, Char, Tuple, Unit, List, Bool, (->)
          -- am I missing something?

      let synonyms :: Synonym'Env
          synonyms = Synonyms.analyze declarations

      -- TODO: I can start with semantic analysis
      let errors = Applied.analyze synonyms declarations
      raise errors -- only of there are any

      let errors = Cycles.analyze synonyms declarations
      raise errors


      let trans'env = TE.Trans'Env{ TE.fixities = fixities, TE.constructors = constructors, TE.fields = fields, TE.kind'context = kind'context, TE.synonyms = synonyms }

      -- TODO: now to run the translation using some sort of run'X function
      run'translate trans'env (to'ast declarations :: Translate [Declaration])


raise :: [Semantic'Error] -> Either Semantic'Error ()
raise [] = Right ()
raise errors = Left $ Many'Errors errors