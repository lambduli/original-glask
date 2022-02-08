module Main (main) where


import Prelude (IO, putStrLn)


import Interpreter.Load (load)


main :: IO ()
main = do
  putStrLn "Glamorous Glask REPL."
  putStrLn ""
  load "prelude.glask"
