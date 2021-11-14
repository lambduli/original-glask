module Main where

import System.IO

import qualified Data.Map.Strict as Map

import Compiler.Parser.Parser (parse'module)

import Compiler.Syntax.ToAST.Translate
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles

import qualified Compiler.Analysis.Syntactic.FixityAnalysis as Fixity
import qualified Compiler.Analysis.Syntactic.ConstructorAnalysis as Constructors
import qualified Compiler.Analysis.Syntactic.SynonymAnalysis as Synonyms

import Compiler.Syntax.ToAST

import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv

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


parse :: String -> Either Semantic'Error [Declaration]
parse source = do
  case parse'module source of
    declarations -> do
      -- TODO: I can start with semantic analysis

      
      -- TODO: now I need to run all the analyses, use them to translate to ast
      -- build the environment for the to'ast translation
      -- also to initialize the translation with some initial state, which should be already prepared somewhere
      let fixities :: Fixity'Env
          fixities = Fixity.analyze declarations

      let -- (constructors, fields) :: (Constr'Env, FIeld'Env)
          (constructors, fields) = Constructors.analyze declarations

      let typing'scope :: Map.Map String Kind
          typing'scope = Map.empty -- TODO: this should not be empty, it needs to contain kinds of all known type constructors
          -- that means primitive types like Int, Char, Tuple, Unit, List, Bool, (->)
          -- am I missing something?

      let synonyms :: Synonym'Env
          synonyms = Synonyms.analyze declarations

      let trans'env = TE.Trans'Env{ TE.fixities = fixities, TE.constructors = constructors, TE.fields = fields, TE.typing'scope = typing'scope, TE.synonyms = synonyms }

      -- TODO: now to run the translation using some sort of run'X function
      run'translate trans'env (to'ast declarations :: Translate [Declaration])
