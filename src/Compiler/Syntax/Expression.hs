module Compiler.Syntax.Expression where


import Data.List ( intercalate )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Literal ( Literal )
import Compiler.Syntax.Type ( Sigma'Type )
import Compiler.Syntax.Declaration ( Declaration )
import Compiler.Syntax.Match ( Match )
import Compiler.Syntax.Pattern ( Pattern )


data Expression
  = Var Name
  | Const Name -- should I have value level Const? I mean, even constructors are just variables so...
  | Op Name -- I might want to remove this, but then I should probably remove Infix'App too
  | Lit Literal
  | Abs Pattern Expression
  | App Expression Expression
  | Infix'App Expression Expression {- Var / Const or rather Op -} Expression
  | Tuple [Expression]
  | If Expression Expression Expression
  | Let [Declaration] Expression
  | Ann Expression Sigma'Type -- (Qualified Type) -- TODO: higher-rank -- should it be qualified type or should it be just Type with the invariant that it's Forall?
  | Case Expression [Match]
  -- | Intro Name [Expression]
  deriving (Eq)


instance Show Expression where
  show (Var name) = name
  show (Const name) = name
  show (Op op) = op
  show (Lit lit) = show lit
  show (Abs pattern'param body) = "(\\ " ++ show pattern'param ++ " -> " ++ show body ++ ")"
  show (App left right) = "(" ++ show left ++ " " ++ show right ++ ")"
  show (Infix'App left op right ) = ""
  show (Tuple exprs) = "(" ++ intercalate ", " (map show exprs) ++ ")"
  show (If cond' then' else') = "if " ++ show cond' ++ " then " ++ show then' ++ " else " ++ show else'
  show (Let decls body) = "let " ++ intercalate "\n" (map show decls) ++ " in " ++ show body
  show (Ann type' expr) = show expr ++ " :: " ++ show type'
  show (Case expr options) = "case " ++ show expr ++ " of \n { " ++ "TODO: show options" ++ " }"
  -- show (Intro name exprs) = "(" ++ name ++ " " ++ unwords (map show exprs) ++ ")"
