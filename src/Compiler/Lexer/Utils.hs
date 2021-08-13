{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Lexer.Utils where


import Control.Monad.State
import Codec.Binary.UTF8.String (encode)


import Compiler.Lexer.LexerState
import Compiler.Lexer.Position
import Compiler.Lexer.Token
import Data.Word


type Number'Of'Chars'Matched = Int


type Matched'Sequence = String


type Lex'Action = Number'Of'Chars'Matched -> Matched'Sequence -> Parser (Maybe Token)


plain'tok :: (Position -> Token) -> Lex'Action
plain'tok tok len _ = do
  pos <- get'position len
  let token = tok pos
  return $ Just token


parametrized'tok :: (a -> Position -> Token) -> (String -> a) -> Lex'Action
parametrized'tok tok read' len matched = do
  pos <- get'position len
  let token = tok (read' matched) pos
  return $ Just token


read'char :: Lex'Action
read'char 2 (c : '\'' : []) = do
  s <- get
  pos <- get'position 2
  put s{ lex'start'code = 0 }
  let token = Tok'Char c pos
  return $ Just token

read'char 3 ('\\' : seq : '\'' : []) = do
  let unesc =
        case seq of
          '\'' -> '\''
          '\\' -> '\\'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          'b' -> '\b'
          'f' -> '\f'
          'v' -> '\v'
          _   -> seq
  s <- get
  pos <- get'position 3
  put s{ lex'start'code = 0 }
  let token = Tok'Char unesc pos
  return $ Just token

read'char _ _ = error "Can never happen."


append'string :: Lex'Action
append'string _ (c : _) = do
  s <- get
  put s{ string'buffer = c : (string'buffer s) }
  return Nothing
append'string _ _ = error "Can never happen."


escape'string :: Lex'Action
escape'string 2 ('\\' : c : []) = do
  let unesc =
        case c of
          {- '\'' -> '\'' -- is this needed? -}
          '"' -> '"'
          '\\' -> '\\'
          'n' -> '\n'
          'r' -> '\r'
          't' -> '\t'
          'b' -> '\b'
          'f' -> '\f'
          'v' -> '\v'
          _   -> c
  s <- get
  put s{ string'buffer = unesc : (string'buffer s) }
  return Nothing
escape'string _ _ = error "Can never happen."


strip'backslash :: Lex'Action
strip'backslash 2 ('\\' : c : []) = do
  s <- get
  put s{ string'buffer = c : (string'buffer s) }
  return Nothing
strip'backslash _ _ = error "Can never happen."


carry'return'string :: Lex'Action
carry'return'string _ _ = do
  s <- get
  put s{ string'buffer = '\n' : (string'buffer s) }
  return Nothing


end'string :: Lex'Action
end'string _ _ = do
  s <- get
  -- (lexSC', prevSCs') <- getPreviousSC
  let buf = string'buffer s
  put  s{ lex'start'code = 0
        , string'buffer = "" }
  let token = Tok'String (reverse buf) (pending'position s)
  return $ Just token



-- The functions that must be provided to Alex's basic interface

-- The input: last character, unused bytes, remaining string
alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte ai =
  case ai'bytes ai of
    (b : bs) ->
      Just (b, ai{ ai'bytes = bs })

    [] ->
      case ai'rest ai of
        [] -> Nothing

        (char : chars) ->
          let
            n = ai'line'number ai
            n' = if char == '\n' then n + 1 else n
            c = ai'column'number ai
            c' = if char == '\n' then 1 else c + 1
            (b : bs) = encode [char]
          in
            Just (b, AlexInput  { ai'prev = char
                                , ai'bytes = bs
                                , ai'rest = chars
                                , ai'line'number = n'
                                , ai'column'number = c' })


get'line'no :: Parser Int
get'line'no = do
  s <- get
  return . ai'line'number . input $ s


get'position :: Int -> Parser Position
get'position tok'len = do
  Parse'State { input = AlexInput { ai'line'number = ln, ai'column'number = cn } } <- get
  return $ Position { line = ln, column = cn - tok'len }


eval'parser :: Parser a -> String -> a
eval'parser parser source = evalState parser (initial'state source)


run'parser :: Parser a -> String -> (a, Parse'State)
run'parser parser source = runState parser (initial'state source)


use'parser :: Parser a -> String -> [a]
use'parser parser source = go [] $ runState parser (initial'state source)
  where
    -- go :: [a] -> (a, Parse'State) -> [a]
    go acc (token, p'state)
      = if finished
        then acc
        else go (token : acc) $ runState parser p'state
      where
        finished = done p'state
          
