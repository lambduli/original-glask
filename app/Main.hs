module Main (main) where


import Prelude (String, IO, putStrLn)


import REPL.Load ( load )
import Compiler.Counter ( Counter(Counter, counter) )
import System.Environment ( getArgs )


main :: IO ()
main = do
  args <- getArgs
  let file'name = module'to'load args
  let cntr = Counter{ counter = 0 }
  putStrLn "Glamorous Glask REPL."
  putStrLn ""
  load file'name cntr


module'to'load :: [String] -> String
module'to'load ("proto" : _) = "protolude.glask"
module'to'load ("p" : file'name : _) = file'name
module'to'load _ = "prelude.glask"
