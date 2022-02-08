module REPL.Repl where


import System.IO
import qualified Data.Map.Strict as Map
import Data.List.Extra (trim)
import Data.Functor.Identity
import Control.Monad.Except

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Program
import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.Type.Infer.Expression

import REPL.ReadExpr
import Compiler.Syntax.ToAST.TranslateEnv
import Compiler.Analysis.Syntactic.Types
import Compiler.Analysis.Error
import Compiler.Syntax
import Compiler.Analysis.TypeSystem.Solver
import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Substitution
import Compiler.Analysis.TypeSystem.Utils.Class
import Compiler.Analysis.TypeSystem.Solver.Composable
import Compiler.Analysis.TypeSystem.Utils.Infer (close'over)


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
          putStrLn "Incorrect Format! The :t command must be followed by an expression, not a declaration."

          -- loop
          repl (program, i'env, cl'env, trans'env, counter)
        Right expression -> do
          let error'or'scheme = infer expression i'env
          -- print
          case error'or'scheme of
            Left err -> do
              putStrLn $ "Type Error: " ++ show err

              -- loop
              repl (program, i'env, cl'env, trans'env, counter)
            Right scheme -> do
              putStrLn $ "         " ++ trim line ++ " :: " ++ show scheme

              -- loop
              repl (program, i'env, cl'env, trans'env, counter)

    -- COMMAND :k(ind)
    ':' : 'k' : line -> do
      putStrLn "<kind checking is not implemented yet>"

    _ -> do
      putStrLn "<expression evaluation is not implemented yet>"


infer :: Expression -> Infer'Env -> Either Error Scheme
infer expr i'env = do
  -- ([Predicate], Type, [Constraint Type], [Constraint Kind])
  (preds, type', cs't, cs'k) <- run'infer i'env (infer'expr expr)

  subst <- run'solve cs't  :: Either Error (Subst T'V Type)

  let Infer'Env{ type'env = t'env, class'env = c'env } = i'env
  
  rs <- runIdentity $ runExceptT $ reduce c'env (apply subst preds) :: Either Error [Predicate]

  -- NOTE:  Maybe I don't need to do any defaulting when inferring just a single Expression and inside the REPL?
  -- TODO:  I need to find out what exactly would that defaulting substitution do to the Expression Type.
  --        I am pretty sure I saw it also do the monomorphisation restriction. I might want to disable that one in the REPL too.
  -- s' <- runIdentity $ runExceptT $ default'subst c'env [] rs :: Either Error (Subst T'V Type)
  let s' = Sub Map.empty

  subst' <- runIdentity $ runExceptT (s' `merge` subst)

  let scheme = close'over $ apply subst' rs :=> apply subst' type'

  return scheme

  -- return $ to'scheme $ apply subst' type'

  -- return (apply subst' $ Map.fromList assumptions, cs'k)
