module Compiler.Analysis.Semantic.SemanticError where

import Data.List.Extra ( intercalate )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Type ( Type )
import Compiler.Syntax.Term.Expression ( Term'Expr )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.ToAST.GSYA.Token ( Token, Op )
import Compiler.Syntax.Term (Term'Pat)


data Semantic'Error
  = Unbound'T'Var Type
  | Not'In'Scope'Data Name
  | Not'In'Scope'Class Name
  | Wrong'Fields Name [Name]
  | Uninitialized'Fields Name [Name]
  | Empty'Record'Update Term'Expr
  | Not'In'Scope'Field Name
  | No'Constructor'Has'All'Fields [Name]
  | Synonym'Not'Fully'Applied Name
  | Synonym'Cycle [(Name, Type)]
  | Many'Errors [Semantic'Error]
  | Ambiguous'Type [Predicate] Type
  | GSYA GSYA'Error
  | Illegal String

  | Internal String


data GSYA'Error
  = Mixing Op Op
  | Unexpected'Expr'Token (Token Term'Expr)
  | Unexpected'Pat'Token (Token Term'Pat)
  | Missing'Operand String


instance Show GSYA'Error where
  show (Mixing op'l op'r) = "Semantic Error: Mixing two operators you shouldn't be mixing."
  show (Unexpected'Expr'Token token) = "Semantic Error: unexpected token '" ++ show token ++ "'"
  show (Unexpected'Pat'Token token) = "Semantic Error: unexpected token '" ++ show token ++ "'"
  show (Missing'Operand message) = "Semantic Error: " ++ message


-- TODO: actually implement proper Show
-- this is really low effort
-- but since it's going to be used mostly by me while making the compiler correct, it's probably good for now
instance Show Semantic'Error where
  show (Unbound'T'Var ty) = "Semantic Error: Unbound Type Variable " ++ show ty
  show (Not'In'Scope'Data s) = "Semantic Error: Data Not In Scope " ++ s
  show (Not'In'Scope'Class c) = "Semantic Error: Type Class Not In Scope " ++ c
  show (Wrong'Fields s ss) = "Semantic Error: Wrong Fields On " ++ s ++ " " ++ show ss
  show (Uninitialized'Fields s ss) = "Semantic Error: Unitialized Fields In " ++ s ++ " " ++ show ss
  show (Empty'Record'Update te) = "Semenatic Error: Empty Record Update (TODO: print the problematic expression)"
  show (Not'In'Scope'Field s) = "Semantic Error: Not In Scope Field " ++ s
  show (No'Constructor'Has'All'Fields ss) = "Semantic Error: No Constructor Has All Fields " ++ show ss
  show (Synonym'Not'Fully'Applied name)
    = "Semantic Error: Type Synonym " ++ name ++ " is not fully applied" -- TODO: add some more info about the problem
  show (Synonym'Cycle aliases)
    = "Semantic Error: Found a cycle in the type synonym declaration(s) of\n" ++ intercalate "\n" (map prnt aliases)
      where
        prnt (name, type') = "  type " ++ name ++ " = " ++ show type'
  show (Many'Errors errs)
    = "Semantic Errors: " ++ intercalate "\n" (map show errs)
  show (Ambiguous'Type preds type')
    = "Semantic Error: Ambiguous type: `" ++ show (preds :=> type') ++ "'"
  show (Internal message)
    = "INTERNAL Semantic Error: " ++ message
  show (GSYA g'err)
    = show g'err
  show (Illegal message)
    = "Illegal - probably something that is not allowed: " ++ show message
