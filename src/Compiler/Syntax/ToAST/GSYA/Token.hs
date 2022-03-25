module Compiler.Syntax.ToAST.GSYA.Token where


import Compiler.Syntax.Fixity ( Fixity(..) )
import Compiler.Syntax.Associativity ( Associativity(Left, None) )


data Token a  = Operator{ fixity :: Fixity, associativity :: Associativity, precedence :: Int, term :: a }
              | Term a


instance Show a => Show (Token a) where
  show (Term a) = show a
  show Operator{ term = t} = show t


data Op = Op{ fix :: Fixity, assoc :: Associativity, prec :: Int }
            deriving (Show)


to'op :: Token a -> Op
to'op Operator{ fixity = f, associativity = a, precedence = p } = Op{ fix = f, assoc = a, prec = p }
to'op _ = error "Internal Error - impossible - to'op called not with Operator."
