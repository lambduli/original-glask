module Interpreter.Repl where


import System.IO
import qualified Data.Map.Strict as Map
import Data.List.Extra (trim)
import Data.Functor.Identity
import Control.Monad.Except


import Compiler.Counter (Counter)


import Interpreter.Expression (read'expr, infer'type)
import Interpreter.Type

import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Analysis.Syntactic.Types
import Compiler.Syntax

import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Program
import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.Type.Infer.Expression

import Compiler.TypeSystem.Error
import Compiler.TypeSystem.Solver
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Utils.Class
import Compiler.TypeSystem.Solver.Composable
import Compiler.TypeSystem.Utils.Infer (close'over)
import Compiler.Syntax.ToAST.TranslateState (Translate'State)


read'cmd'or'expr :: IO String
read'cmd'or'expr = do
  putStr "glask λ > "
  hFlush stdout
  line <- getLine
  case line of
    "" -> return line
    ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : _ -> return line
    ':' : 'q' : _ -> return line
    ':' : 'Q' : _ -> return line
    _ -> do
      next'line <- read'expr'
      return $ line ++ ['\n'] ++ next'line
    where
      read'expr' = do
        putStr "         "
        hFlush stdout
        line <- getLine
        case line of
          "" -> return line
          ':' : 'e' : 'x' : 'i' : 't' : _ -> return line
          ':' : 'l' : 'o' : 'a' : 'd' : ' ' : _ -> return line
          ':' : 'q' : _ -> return line
          ':' : 'Q' : _ -> return line
          _ -> do
            next'line <- read'expr'
            return $ line ++ ['\n'] ++ next'line


repl :: (Program, Infer'Env, Class'Env, Translate'Env, Counter) -> IO ()
repl (program, i'env@Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }, cl'env, trans'env, counter) = do
  -- read
  line <- read'cmd'or'expr

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl (program, i'env, cl'env, trans'env, counter)
    ":exit" -> do
      putStrLn "Bye!"
      return ()
    ':' : 'l' : 'o' : 'a' : 'd' : ' ' : file -> do
      let trimmed = trim file
      putStr "<loading arbitrary files not implemented yet>"

    ":q" -> do
      putStrLn "Bye!"
      return ()
    ":Q" -> do
      putStrLn "Bye!"
      return ()

    -- COMMAND :t(ype)
    ':' : 't' : line -> do
      case read'expr line trans'env counter of
        Left _ -> do
          putStrLn "Incorrect Format! The :t command must be followed by an expression."

          -- loop
          repl (program, i'env, cl'env, trans'env, counter)
        Right (expression, counter') -> do
          let error'or'scheme = infer'type expression i'env counter'
          -- print
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl (program, i'env, cl'env, trans'env, counter)
            Right (scheme, counter'') -> do
              putStrLn $ "          " ++ trim line ++ " :: " ++ show scheme

              -- loop
              repl (program, i'env, cl'env, trans'env, counter'')

    -- COMMAND :k(ind)
    ':' : 'k' : line -> do
      case read'type line trans'env counter of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          -- putStrLn "Incorrent Format! The :k command must be followed by an type exression."

          -- loop
          repl (program, i'env, cl'env, trans'env, counter)
        
        Right (type', counter') -> do
          let error'or'kind = infer'kind type' i'env counter'
          -- print
          case error'or'kind of
            Left err -> do
              putStrLn $ "Kind Error: " ++ show err

              -- loop
              repl (program, i'env, cl'env, trans'env, counter')

            Right (kind, counter'') -> do
              putStrLn $ "          " ++ trim line ++ " :: " ++ show kind

              -- loop
              repl (program, i'env, cl'env, trans'env, counter'')


    _ -> do
      putStrLn "<expression evaluation is not implemented yet>"

