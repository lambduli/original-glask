module ExamplesSpec where


import Test.Hspec

import System.IO

import qualified Data.Map.Strict as Map
import Data.Maybe ( isNothing )

import Control.Monad.Extra ( allM )
import Control.Monad.State ( runState )


import Compiler.Syntax.Term
import Compiler.Syntax.Literal
import Compiler.Syntax.Type ( Sigma'Type, Type )

import Compiler.Parser.Parser ( parse'module, parse'decls, parse'expr, parse'type )

import Compiler.TypeSystem.Error ( Error )

import Compiler.TypeSystem.InferenceEnv ( init'k'env, class'env )
import Compiler.TypeSystem.InferenceEnv as I'Env
import qualified Compiler.TypeSystem.InferenceState as I'State
import Compiler.TypeSystem.InferenceState ( Infer'State )

import qualified Interpreter.Core as Core
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import Compiler.Syntax.ToAST.TranslateEnv

import Interpreter.Evaluate ( eval )
import Interpreter.ToCore ( decls'to'core, section'to'core, data'to'core, to'core )


import Compiler.Counter

import REPL.Expression ( read'expr, infer'type, infer'expr'type )
import REPL.Load ( load'declarations, process'declarations, build'env'store )
import Interpreter.Value (Value (Literal))
import Compiler.Syntax (Expression, Literal (Lit'Int))



import Compiler.TypeSystem.Program ( Program(Program, b'sec'core, environment, store) )
import Compiler.TypeSystem.Program ( Program( Program, data'declarations, bind'section, methods, method'annotations, b'sec'core, environment, store) )


spec :: Spec
spec = do
  describe "Testing './examples' folder for analysis related stuff" $ do

    let file = "./examples/positive/annotations/forall.glask"
    it file $ do
      r <- type'check "./examples/positive/annotations/forall.glask"
      r `shouldBe` Nothing


    let file = "./examples/positive/classes/class.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing
    
    let file = "./examples/positive/classes/higherkinded.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing
    
    let file = "./examples/positive/classes/qualified.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing


    let file = "./examples/positive/data/bool.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

    let file = "./examples/positive/data/phantom.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

    let file = "./examples/positive/data/records.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing


    let file = "./examples/positive/higher-kinded/higherkinded.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing    

    let file = "./examples/positive/higher-kinded/lists.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing


    let file = "./examples/positive/operators/infix.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

    let file = "./examples/positive/operators/prefix.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

    let file = "./examples/positive/operators/prefix-postfix.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

    let file = "./examples/positive/operators/implicit.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing


    let file = "./examples/positive/higher-rank/basic.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

    let file = "./examples/positive/higher-rank/data.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing
    
    let file = "./examples/positive/higher-rank/case.glask"
    it file $ do
      r <- type'check file
      r `shouldBe` Nothing

  describe "Testing './examples' folder for translation/evaluation related stuff" $ do
    let file = "./examples/positive/evaluate/arith.glask"
    it file $ do
      r <- eval'within "2 + 3" file
      r `shouldBe` Right (Literal (Lit'Int 5))






type'check :: String -> IO (Maybe String)
type'check file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> return $ Just $ show sem'err

    Right (decls, trans'env, counter') -> do
      case process'declarations decls trans'env counter' of
        Left err -> return $ Just $ show err

        Right (program, infer'env, trans'env, counter'', infer'state) -> do
          return Nothing
          -- let k'e = kind'env infer'env

          -- let b'section = bind'section program
          -- let b'sec'core = section'to'core b'section

          -- let data'decls = data'declarations program

          -- -- TODO: I also need to add constructor for () and [] and (,) and (,,) and so on
          -- let unit'constr = Core.Binding "()" (Core.Intro "()" [])
          -- let cons'constr = Core.Binding ":" (Core.Abs "a" (Core.Abs "as" (Core.Intro ":" [Core.Var "a", Core.Var "as"])))
          -- let nil'constr  = Core.Binding "[]" (Core.Intro "[]" [])
          -- let constructor'core = unit'constr : cons'constr : nil'constr : data'to'core data'decls

          -- let (env, stor) = build'env'store $ b'sec'core ++ constructor'core


  where counter   = Counter { counter = 0 }


eval'within :: String -> String -> IO (Either String Value)
eval'within expr file'name = load file'name counter expr
  where counter   = Counter { counter = 0 }


load :: String -> Counter -> String -> IO (Either String Value)
load file'name counter expr = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> do
      return $ Left $ show sem'err

    Right (decls, trans'env, counter') -> do

      case process'declarations decls trans'env counter' of
        Left err -> do
          return $ Left $ show err

        Right (program, infer'env, trans'env, counter'', infer'state) -> do
          let k'e = kind'env infer'env

          let b'section = bind'section program
          let b'sec'core = section'to'core b'section

          let data'decls = data'declarations program


          let unit'constr = Core.Binding "()" (Core.Intro "()" [])
          let cons'constr = Core.Binding ":" (Core.Abs "a" (Core.Abs "as" (Core.Intro ":" [Core.Var "a", Core.Var "as"])))
          let nil'constr  = Core.Binding "[]" (Core.Intro "[]" [])
          let constructor'core = unit'constr : cons'constr : nil'constr : data'to'core data'decls

          let (env, stor) = build'env'store $ b'sec'core ++ constructor'core

          repl'expr expr (program{ b'sec'core = b'sec'core ++ constructor'core, environment = env, store = stor }, infer'env, trans'env{ TE.kind'context = k'e `Map.union` (TE.kind'context trans'env)}, counter'', infer'state)


repl'expr :: String -> (Program, Infer'Env, Translate'Env, Counter, Infer'State Type) -> IO (Either String Value)
repl'expr expr (program@Program{ environment = environment, store = store }, i'env@Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }, trans'env, counter, infer'state) = do
  case read'expr expr trans'env counter of
    Left trans'err -> do
      return $ Left $ show trans'err

    Right (expr, counter''') -> do
      let inf'env = i'env{ I'Env.instances = I'State.instances infer'state, I'Env.overloaded = I'State.overloaded infer'state }

      let error'or'scheme = infer'expr'type expr inf'env counter'''

      case error'or'scheme of
        Left err -> do
          return $ Left $ show err

        Right (_, expr', counter'''') -> do
          let core'expr = to'core expr'
          let (res, store') = runState (eval core'expr environment) store
          case res of
            Left eval'err ->
              return $ Left $ show eval'err

            Right value ->
              return $ Right value
  