module LexerSpec where


import Test.Hspec

import Data.Function

import Compiler.Lexer
import Compiler.Lexer.Token
import Compiler.Lexer.Position
import Compiler.Lexer.Located
import Compiler.Lexer.Utils


infix 5 |=>

(|=>) :: String -> (Position -> Token) -> Bool
source |=> place'tok = produces
  where
    lexeme = eval'parser read'token source
    pos = at lexeme :: Position
    produces = lexeme == place'tok pos


infix 5 ~:

(~:) :: [Position -> Token] -> String -> Bool
place'toks ~: source = equivalent
  where
    parsed'tokens = use'parser read'token source
    equivalent = and $ zipWith (~~) parsed'tokens (map ($ None) place'toks)

-- (~:) :: [Position -> Token] -> String -> Bool
-- place'toks ~: source = equaivalent
--   where
--     parsed'tokens = use'parser read'token source
--     references = zipWith ((&) . at) parsed'tokens place'toks
--     all'same = zipWith (==) references parsed'tokens
--     equaivalent = all all'same


spec :: Spec
spec = do
  describe "Test reading keywords" $ do

    it "reads 'data'" $ do
      "data" |=> Tok'Data

    it "reads 'if'" $ do
      "if" |=> Tok'If

    it "reads 'then'" $ do
      "then" |=> Tok'Then

    it "reads 'else'" $ do
      "else" |=> Tok'Else

    it "reads 'let'" $ do
      "let" |=> Tok'Let

    it "reads 'in'" $ do
      "in" |=> Tok'In

    it "reads 'case'" $ do
      "case" |=> Tok'Case

    it "reads 'of'" $ do
      "of" |=> Tok'Of

    it "reads 'type'" $ do
      "type" |=> Tok'Type

    it "reads '_'" $ do
      "_" |=> Tok'Named'Hole "_"

    it "reads '\\'" $ do
      "\\" |=> Tok'Lambda

    it "reads 'class'" $ do
      "class" |=> Tok'Class

    it "reads 'instance'" $ do
      "instance" |=> Tok'Instance

    it "reads 'where'" $ do
      "where" |=> Tok'Where

    it "reads 'module'" $ do
      "module" |=> Tok'Module

    it "reads '::'" $ do
      "::" |=> Tok'Has'Type

    it "reads a decimal number" $ do
      "23" |=> Tok'Int 23

  describe "Test reading identifiers" $ do

    it "reads a simple alphabetical variable identifier" $ do
      "identifier" |=> Tok'Ident'Var "identifier"

    it "reads a variable identifier with apostrophe" $ do
      "id'" |=> Tok'Ident'Var "id'"

    it "reads a constant identifier" $ do
      "Int" |=> Tok'Ident'Const "Int"

    it "reads a constant identifier with apostrophe" $ do
      "Special'Type" |=> Tok'Ident'Const "Special'Type"

    it "reads a variable operator identifier" $ do
      "+" |=> Tok'Operator "+"

    it "reads a variable operator identifier containing digits" $ do
      ">3<5" |=> Tok'Operator ">3<5"

    it "reads a variable operator identifier containing letters" $ do
      "+mmo:" |=> Tok'Operator "+mmo:"

    it "reads a simple constant operator identifier" $ do
      ":" |=> Tok'Operator'Const ":"

    it "reads a constant operator identifier" $ do
      ":=>" |=> Tok'Operator'Const ":=>"

  describe "Test reading special symbols" $ do

    it "reads '('" $ do
      "(" |=> Tok'Left'Paren



  describe "Test reading sequence of tokens" $ do

    -- this one passes when the list is empty even though it should
    it "reads a simple arithmetic operation" $ do
      [Tok'Int 23, Tok'Operator "+", Tok'Int 42] ~: "23 + 42"

