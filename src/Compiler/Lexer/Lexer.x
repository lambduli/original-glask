{
module Compiler.Lexer.Lexer (lexer, read'token) where

import Control.Monad.State

import Compiler.Lexer.LexerState
import Compiler.Lexer.Token
import Compiler.Lexer.Position
import Compiler.Lexer.Utils
}


$digit                = [0-9]
$octit                = [0-7]
$hexit                = [a-f A-F $digit]

@decimal              = $digit+
@octal                = $octit+
@hexadecimal          = $hexit+
@exponent             = [eE] [\-\+] @decimal

$lower                = [a-z]
$upper                = [A-Z]

$symbol               = [ \! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ \: ]
$opstart              = [ \! \# \$ \% \& \* \+ \. \/ \< \= \> \? \@ \\ \^ \| \- \~ ]

$idchar               = [$lower $upper $digit $symbol \']
$opchar               = [$symbol $lower $upper $digit \:]
-- TODO: Should operators contain letters and numbers?

@variableident        = $lower $idchar*
@constrident          = $upper $idchar*

@operator             = $opstart $opchar*
@opconstr             = \: $opchar*


$space                = [\ \t\f\v]


--
-- lexical grammar
--

token :-

-- reserved keywords
<0>         data                        { plain'tok Tok'Data }
<0>         if                          { plain'tok Tok'If }
<0>         then                        { plain'tok Tok'Then }
<0>         else                        { plain'tok Tok'Else }
<0>         let                         { plain'tok Tok'Let }
<0>         in                          { plain'tok Tok'In }
<0>         case                        { plain'tok Tok'Case }
<0>         of                          { plain'tok Tok'Of }
<0>         type                        { plain'tok Tok'Type }
<0>         \_                          { plain'tok Tok'Underscore }
<0>         "\"                         { plain'tok Tok'Lambda }
<0>         class                       { plain'tok Tok'Class }
<0>         instance                    { plain'tok Tok'Instance }
<0>         where                       { plain'tok $ Tok'Where }
<0>         module                      { plain'tok $ Tok'Module }
<0>         infixl                      { plain'tok $ Tok'Infixl }
<0>         infix                       { plain'tok $ Tok'Infix }
<0>         infixr                      { plain'tok $ Tok'Infixr }
<0>         forall                      { plain'tok $ Tok'Forall }


-- special symbols
-- ( ) , [ ] ` { }
-- <0>         "(" $space* ")"          { parametrized'tok Tok'Var'Upper (const "()") }
<0>         "("                         { plain'tok Tok'Left'Paren }
<0>         ")"                         { plain'tok Tok'Right'Paren }
<0>         "[" $space* "]"             { parametrized'tok Tok'Ident'Const (const "[]") }
<0>         "["                         { plain'tok Tok'Left'Bracket }
<0>         "]"                         { plain'tok Tok'Right'Bracket }
<0>         ","                         { plain'tok Tok'Comma }
<0>         "`"                         { plain'tok Tok'Backtick }
<0>         "{"                         { plain'tok Tok'Left'Brace }
<0>         "}"                         { plain'tok Tok'Right'Brace }
<0>         ";"                         { plain'tok Tok'Semicolon }
<0>         "::"                        { plain'tok Tok'Has'Type }


-- variables and constructors
<0>         @variableident              { parametrized'tok Tok'Ident'Var id }
<0>         @constrident                { parametrized'tok Tok'Ident'Const id }
<0>         @operator                   { parametrized'tok Tok'Operator id }
<0>         @opconstr                   { parametrized'tok Tok'Operator'Const id }


<0>         \n                          ;
<0>         $space+                     ;


-- literals

<0>         @decimal
          | \-@decimal
          | 0[oO] @octal
          | \-0[oO] @octal
          | 0[xX] @hexadecimal
          | \-0[xX] @hexadecimal        { parametrized'tok Tok'Int read }

<0>         @decimal \. @decimal @exponent?
          | \-@decimal \. @decimal @exponent?
          | @decimal @exponent
          | \-@decimal @exponent        { parametrized'tok Tok'Double read }


<0>         \'                          { begin'char }
<char'SC>    .{1, 2}\'                  { read'char }

<0>         \"                          { begin'string }
<string'SC>  \"                         { end'string }
<string'SC>  \\['\"\\nrtbfv]            { escape'string }
<string'SC>  \\.                        { strip'backslash }
<string'SC>  .                          { append'string }


-- rest-of-the-line comment
<0>         \-\-.*\n                    ;


{

begin'char :: Lex'Action
begin'char _ _ = do
  s <- get
  put s{ lex'start'code = char'SC }
  return Nothing


begin'string :: Lex'Action
begin'string len _ = do
  s <- get
  pos <- get'position len
  put  s{ lex'start'code = string'SC, pending'position = pos }
  return Nothing


-- TODO:
-- I need to find out how to use the state for better lexical-error messages
read'token :: Parser Token
read'token = do
  s <- get

  -- NOTE: If I generate extra tokens for off-side rule 
  -- I will need to keep actual tokens pending before the extras are read
  -- Those have to be read now.
  case pending'tokens s of
    tok : toks -> do
      put s{ pending'tokens = toks }
      return tok

    [] ->
      case alexScan (input s) (lex'start'code s) of
        AlexEOF -> do
          pos <- get'position 0
          put s{ done = True }
          return $ Tok'EOF pos

        AlexError inp' -> error $ "Lexical error on line " ++ (show $ ai'line'number inp')
        
        AlexSkip inp' _ -> do
          put s{ input = inp' }
          read'token
        
        AlexToken inp' n act -> do
          -- let ll = layout'stack s
          let (AlexInput{ ai'rest = buf }) = input s
          put s{ input = inp' }
          res <- act n (take n buf)
          case res of
            Nothing -> read'token
            Just t -> return t


lexer :: (Token -> Parser a) -> Parser a
lexer cont = do
  tok <- read'token
  cont tok

}