module Compiler.Syntax.Literal where


data Literal
  = Lit'Int Int
  | Lit'Double Double
  | Lit'Char Char
  deriving (Eq, Ord)

  -- I can safely derive and use Ord because the type of Glask's comparision operations
  -- prohibits from comparing two literals of different types
  -- Thanks to this, the Interpreter can just compare literals without caring too much.
  -- I am making some assumptions, they should be safe to make.


instance Show Literal where
  show (Lit'Int i) = show i
  show (Lit'Double d) = show d
  show (Lit'Char ch) = show ch