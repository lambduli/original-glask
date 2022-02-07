module Main where


import REPL.Load


main :: IO ()
main = do
  putStrLn "Glamorous Glask REPL."
  putStrLn ""
  load "prelude.glask"

