module Main (main) where


import Prelude (IO, putStrLn)


import Interpreter.Load (load)
import Compiler.Counter (Counter(Counter, counter))


main :: IO ()
main = do
  let cntr = Counter{ counter = 0 }
  putStrLn "Glamorous Glask REPL."
  putStrLn ""
  load "prelude.glask" cntr
