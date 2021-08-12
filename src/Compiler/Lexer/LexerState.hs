module Compiler.Lexer.LexerState where


import Control.Monad.State
import Control.Monad
import Data.Word

import Compiler.Lexer.Token
import Compiler.Lexer.Position


-- TODO: CONSIDER: If I change it to ExceptT I can report specific errors
-- do I need that though?
type Parser a = State Parse'State a


data AlexInput = AlexInput
  { ai'prev           :: Char
  , ai'bytes          :: [Word8]
  , ai'rest           :: String
  , ai'line'number    :: Int
  , ai'column'number  :: Int }
  deriving Show


data Parse'State = Parse'State
  { input             :: AlexInput
  , lex'start'code    :: Int              -- lexer start code
  , string'buffer     :: String           -- temporary storage for strings
  , pending'tokens    :: [Token]          -- for when Parser consumes the lookeahead and decided to put it back
  , pending'position  :: Position }       -- needed when parsing strings, chars, multi-line strings
  deriving Show


initial'state :: String -> Parse'State
initial'state s = Parse'State
  { input = AlexInput
    { ai'prev = '\n'
    , ai'bytes = []
    , ai'rest = s
    , ai'line'number = 1
    , ai'column'number = 1 }
  , lex'start'code = 0
  , string'buffer = ""
  , pending'tokens = []
  , pending'position = Position { line = 1, column = 1 }}
