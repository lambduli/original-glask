module REPL.Repl where


import System.IO
import qualified Data.Map.Strict as Map
import Data.List.Extra ( trim )
import Data.Functor.Identity
import Control.Monad.Except
import Control.Monad.State ( runState )


import Compiler.Counter ( Counter )


import REPL.Expression ( read'expr, infer'type, infer'expr'type )
import REPL.Type

import Compiler.Syntax
import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Analysis.Syntactic.Types
import Compiler.Syntax.ToAST.TranslateState ( Translate'State )

import Compiler.TypeSystem.Program ( Program(Program, b'sec'core, environment, store) )
import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Program
import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.ClassEnv ( Class'Env(..) )
import Compiler.TypeSystem.Type.Infer.Expression

import Compiler.TypeSystem.Error
import Compiler.TypeSystem.Solver
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Utils.Class
import Compiler.TypeSystem.Solver.Composable
import Compiler.TypeSystem.InferenceState ( Infer'State )
import qualified Compiler.TypeSystem.InferenceState as I'State
import Compiler.TypeSystem.InferenceEnv ( Infer'Env )
import Compiler.TypeSystem.InferenceEnv as I'Env


import Interpreter.Evaluate ( eval )
import Interpreter.ToCore ( to'core )


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


repl :: (Program, Infer'Env, Translate'Env, Counter, Infer'State Type) -> IO ()
repl (program@Program{ environment = environment, store = store }, i'env@Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }, trans'env, counter, infer'state) = do
  -- read
  line <- read'cmd'or'expr

  -- evaluate
  case line of
    [] -> do
      putStrLn ""

      -- loop
      repl (program, i'env, trans'env, counter, infer'state)
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
          repl (program, i'env, trans'env, counter, infer'state)
        Right (expression, counter') -> do
          let error'or'scheme = infer'type expression i'env counter'
          -- print
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl (program, i'env, trans'env, counter, infer'state)
            Right (scheme, _, counter'') -> do
              putStrLn $ "          " ++ trim line ++ " :: " ++ show scheme

              -- loop
              repl (program, i'env, trans'env, counter'', infer'state)

    -- COMMAND :k(ind)
    ':' : 'k' : line -> do
      case read'type line trans'env counter of
        Left err -> do
          putStrLn $ "Error: " ++ show err
          -- putStrLn "Incorrent Format! The :k command must be followed by an type exression."

          -- loop
          repl (program, i'env, trans'env, counter, infer'state)
        
        Right (type', counter') -> do
          let error'or'kind = infer'kind type' i'env counter'
          -- print
          case error'or'kind of
            Left err -> do
              putStrLn $ "Kind Error: " ++ show err

              -- loop
              repl (program, i'env, trans'env, counter', infer'state)

            Right (kind, counter'') -> do
              putStrLn $ "          " ++ trim line ++ " :: " ++ show kind

              -- loop
              repl (program, i'env, trans'env, counter'', infer'state)

    -- _ -> do
    --   -- here the evaluation will happen
    --   -- I have the environment and store
    --   -- so just read the expression, convert it to Core
    --   -- evaluate it within the env and store
    --   -- 
    --   -- read'expr :: String -> Translate'Env -> Translate'State -> Either Semantic'Error (Expression, Translate'State)
    --   case read'expr line trans'env counter of
    --     Left trans'err -> do
    --       putStrLn $ "Error: " ++ show trans'err
    --     Right (expr, counter''') -> do
    --       -- todo: I need to type check the expression
    --       putStrLn $ "Expression: " ++ show expr

    --       putStrLn $ "\n\n              evaluation environment: " ++ show environment
    --       putStrLn $ "\n\n              evaluation pointer 8: " ++ show (store Map.!? 8)
    --       putStrLn $ "\n\n              evaluation pointer 10: " ++ show (store Map.!? 10)


    --       -- NOTE: before I run the infer'type, I need to modify the i'env
    --       -- I need to add all overloads and instances form the previous state into it
    --       let inf'env = i'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
    --       putStrLn $ "|takze jak teda vypada inf'env? " ++ show inf'env ++ "|\n|\n|\n"
    --       -- putStrLn $ "\n\n\n\n\n\n\n\n\n\n............. i'env: " ++ show i'env ++ "\n\n\n\n\n\n\n"

    --       -- let error'or'scheme = infer'type expr inf'env counter'''
    --       -- print
    --       -- case error'or'scheme of
    --         -- Left err -> do
    --           -- putStrLn $ "Type Error: " ++ show err
    --           -- loop
    --           -- repl (program, i'env, trans'env, counter''', infer'state)
    --         -- Right (_, expr', counter'''/) -> do
    --       let core'expr = to'core expr
    --       putStrLn $ "Elaborated Expression: " ++ show expr
    --       putStrLn $ "Core Expression: " ++ show core'expr
    --       let (res, store') = runState (eval core'expr environment) store
    --       case res of
    --         Left eval'err -> do
    --           putStrLn $ "Evaluation Error: " ++ show eval'err
    --         Right value -> do
    --           let hacky'serialized = show value
    --           putStrLn "Evaluated to: "
    --           putStrLn hacky'serialized
              
    --           repl (program{ store = store' }, i'env, trans'env, counter''', infer'state)
    --           -- putStrLn "<expression evaluation is not implemented yet>"


    _ -> do
      -- here the evaluation will happen
      -- I have the environment and store
      -- so just read the expression, convert it to Core
      -- evaluate it within the env and store
      -- 
      -- read'expr :: String -> Translate'Env -> Translate'State -> Either Semantic'Error (Expression, Translate'State)
      case read'expr line trans'env counter of
        Left trans'err -> do
          putStrLn $ "Error: " ++ show trans'err
        Right (expr, counter''') -> do
          -- todo: I need to type check the expression
          putStrLn $ "Expression: " ++ show expr

          putStrLn $ "\n\n              evaluation environment: " ++ show environment
          putStrLn $ "\n\n              evaluation pointer 8: " ++ show (store Map.!? 8)
          putStrLn $ "\n\n              evaluation pointer 10: " ++ show (store Map.!? 10)


          -- NOTE: before I run the infer'type, I need to modify the i'env
          -- I need to add all overloads and instances form the previous state into it
          let inf'env = i'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }
          putStrLn $ "|takze jak teda vypada inf'env? " ++ show inf'env ++ "|\n|\n|\n"
          -- putStrLn $ "\n\n\n\n\n\n\n\n\n\n............. i'env: " ++ show i'env ++ "\n\n\n\n\n\n\n"

          let error'or'scheme = infer'expr'type expr inf'env counter'''
          -- print
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err
              -- loop
              repl (program, i'env, trans'env, counter''', infer'state)
            Right (_, expr', counter'''') -> do
              let core'expr = to'core expr'
              putStrLn $ "Elaborated Expression: " ++ show expr'
              putStrLn $ "Core Expression: " ++ show core'expr
              let (res, store') = runState (eval core'expr environment) store
              case res of
                Left eval'err -> do
                  putStrLn $ "Evaluation Error: " ++ show eval'err
                Right value -> do
                  let hacky'serialized = show value
                  putStrLn "Evaluated to: "
                  putStrLn hacky'serialized
                  
                  repl (program{ store = store' }, i'env, trans'env, counter'''', infer'state)
                  -- putStrLn "<expression evaluation is not implemented yet>"

