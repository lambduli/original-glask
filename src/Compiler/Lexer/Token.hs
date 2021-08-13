module Compiler.Lexer.Token where


import Compiler.Lexer.Position
import Compiler.Lexer.Located


data Token
  -- keywords
  = Tok'Data Position       -- data
  | Tok'If Position         -- if
  | Tok'Then Position       -- then
  | Tok'Else Position       -- else
  | Tok'Let Position        -- let
  | Tok'In Position         -- in
  | Tok'Case Position       -- case
  | Tok'Of Position         -- of
  | Tok'Type Position       -- type
  | Tok'Underscore Position -- _
  | Tok'Lambda Position     -- \
  | Tok'Class Position      -- class
  | Tok'Instance Position   -- instance
  | Tok'Where Position      -- where
  | Tok'Module Position     -- module
  | Tok'Has'Type Position   -- ::

  -- forms of identifiers
  | Tok'Ident'Var String Position
  | Tok'Ident'Const String Position
  | Tok'Operator String Position
  | Tok'Operator'Const String Position

  -- special symbols
  | Tok'Left'Paren Position     -- (
  | Tok'Right'Paren Position    -- )
  | Tok'Left'Bracket Position   -- [
  | Tok'Right'Bracket Position  -- ]
  | Tok'Comma Position          -- ,
  | Tok'Backtick Position       -- `
  | Tok'Semicolon Position      -- ;
  | Tok'Left'Brace Position     -- {
  | Tok'Right'Brace Position    -- }

  -- literals
  | Tok'Int Int Position
  | Tok'Double Double Position
  | Tok'Char Char Position
  | Tok'String String Position

  | Tok'EOF Position
  deriving (Eq, Show)


instance Located Token where
  at (Tok'Data position) = position
  at (Tok'If position) = position
  at (Tok'Then position) = position
  at (Tok'Else position) = position
  at (Tok'Let position) = position
  at (Tok'In position) = position
  at (Tok'Case position) = position
  at (Tok'Of position) = position
  at (Tok'Type position) = position
  at (Tok'Underscore position) = position
  at (Tok'Lambda position) = position
  at (Tok'Class position) = position
  at (Tok'Instance position) = position
  at (Tok'Where position) = position
  at (Tok'Module position) = position
  at (Tok'Has'Type position) = position

  at (Tok'Ident'Var _ position) = position
  at (Tok'Ident'Const _ position) = position
  at (Tok'Operator _ position) = position
  at (Tok'Operator'Const _ position) = position
  at (Tok'Left'Paren position) = position
  at (Tok'Right'Paren position) = position
  at (Tok'Left'Bracket position) = position
  at (Tok'Right'Bracket position) = position
  at (Tok'Comma position) = position
  at (Tok'Backtick position) = position
  at (Tok'Semicolon position) = position
  at (Tok'Left'Brace position) = position
  at (Tok'Right'Brace position) = position

  at (Tok'Int _ position) = position
  at (Tok'Double _ position) = position
  at (Tok'Char _ position) = position
  at (Tok'String _ position) = position

  at (Tok'EOF position) = position



{- Following operator is usefull for testing. It allows to compare only by the tag. -}

infix 4 ~~

(~~) :: Token -> Token -> Bool
(Tok'Data _) ~~ (Tok'Data _) = True
(Tok'If _) ~~ (Tok'If _) = True
(Tok'Then _) ~~ (Tok'Then _) = True
(Tok'Else _) ~~ (Tok'Else _) = True
(Tok'Let _) ~~ (Tok'Let _) = True
(Tok'In _) ~~ (Tok'In _) = True
(Tok'Case _) ~~ (Tok'Case _) = True
(Tok'Of _) ~~ (Tok'Of _) = True
(Tok'Type _) ~~ (Tok'Type _) = True
(Tok'Underscore _) ~~ (Tok'Underscore _) = True
(Tok'Lambda _) ~~ (Tok'Lambda _) = True
(Tok'Class _) ~~ (Tok'Class _) = True
(Tok'Instance _) ~~ (Tok'Instance _) = True
(Tok'Where _) ~~ (Tok'Where _) = True
(Tok'Module _) ~~ (Tok'Module _) = True
(Tok'Has'Type _) ~~ (Tok'Has'Type _) = True
(Tok'Ident'Var _ _) ~~ (Tok'Ident'Var _ _) = True
(Tok'Ident'Const _ _) ~~ (Tok'Ident'Const _ _) = True
(Tok'Operator _ _) ~~ (Tok'Operator _ _) = True
(Tok'Operator'Const _ _) ~~ (Tok'Operator'Const _ _) = True
(Tok'Left'Paren _) ~~ (Tok'Left'Paren _) = True
(Tok'Right'Paren _) ~~ (Tok'Right'Paren _) = True
(Tok'Left'Bracket _) ~~ (Tok'Left'Bracket _) = True
(Tok'Right'Bracket _) ~~ (Tok'Right'Bracket _) = True
(Tok'Comma _) ~~ (Tok'Comma _) = True
(Tok'Backtick _) ~~ (Tok'Backtick _) = True
(Tok'Semicolon _) ~~ (Tok'Semicolon _) = True
(Tok'Left'Brace _) ~~ (Tok'Left'Brace _) = True
(Tok'Right'Brace _) ~~ (Tok'Right'Brace _) = True
(Tok'Int _ _) ~~ (Tok'Int _ _) = True
(Tok'Double _ _) ~~ (Tok'Double _ _) = True
(Tok'Char _ _) ~~ (Tok'Char _ _) = True
(Tok'String _ _) ~~ (Tok'String _ _) = True
(Tok'EOF _) ~~ (Tok'EOF _) = True
_ ~~ _ = False
