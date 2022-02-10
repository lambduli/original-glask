module Compiler.Analysis.Semantic.SemanticError where

import Data.List.Extra (intercalate)


import Compiler.Syntax.Name
import Compiler.Syntax.Type
import Compiler.Syntax.Term.Expression


data Semantic'Error
  = Unbound'T'Var Type
  | Not'In'Scope'Data Name
  | Wrong'Fields Name [Name]
  | Uninitialized'Fields Name [Name]
  | Empty'Record'Update Term'Expr
  | Not'In'Scope'Field Name
  | No'Constructor'Has'All'Fields [Name]
  | Synonym'Not'Fully'Applied Name
  | Synonym'Cycle [(Name, Type)]
  | Many'Errors [Semantic'Error]

  | Internal String



-- TODO: actually implement proper Show
-- this is really low effort
-- but since it's going to be used mostly by me while making the compiler correct, it's probably good for now
instance Show Semantic'Error where
  show (Unbound'T'Var ty) = "Semantic Error: Unbound Type Variable " ++ show ty
  show (Not'In'Scope'Data s) = "Semantic Error: Data Not In Scope " ++ s
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
  show (Internal message)
    = "INTERNAL Semantic Error: " ++ message
