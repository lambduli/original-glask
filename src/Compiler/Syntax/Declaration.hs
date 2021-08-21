module Compiler.Syntax.Declaration where

import Data.List


import {-# SOURCE #-} Compiler.Syntax.Expression
import Compiler.Syntax.Type
import Compiler.Syntax.Qualified
import Compiler.Syntax.Predicate
import Compiler.Syntax.Class
import Compiler.Syntax.Instance
import Compiler.Syntax.Signature
import Compiler.Syntax.Name
import Compiler.Syntax.BindGroup
import {-# SOURCE #-} Compiler.Syntax.Term


data Declaration
  = Binding (Either (Term'Pat, Term'Expr) Bind'Group)  -- id x = x
  -- I may need to add information about whether it is explicitly typed or not

  -- | Annotated String Type Expression        -- id :: a -> a ; id x = x
  -- I think this one actually might not be that usefull
  -- I can use combination of Signature and Binding
  | Signature Signature -- Name (Qualified Type)           -- id :: a -> a
  | Data'Decl Name [Name] [Constr'Decl]       -- Data type declaration -- name type'params list'of'consturctors'with'params
  | Type'Alias Name Type                      -- type String = List Char
  | Fixity Fixity Int Name                    -- infix 5 +
  | Class Name Name [Predicate] [Declaration] -- class (Super1 a1, ... , SuperN aN) ==> Name where { list of Signatures }
  --    cname parname supers     signatures
  | Instance Instance [Declaration]           -- instance ... where { list of Bindings }
  deriving (Eq)


data Fixity
  = Infixl
  | Infix
  | Infixr
  deriving (Eq)


instance Show Fixity where
  show Infixl = "infixl"
  show Infix = "infix"
  show Infixr = "infixr"


instance Show Declaration where
  show (Binding bind'group)
    = "TODO: show bind'group"
    -- = name ++ " = " ++ show expr
  -- show (Annotated name type' expr)
    -- = name ++ " :: " ++ show type' ++ "\n" ++ name ++ " = " ++ show expr
  show (Signature (T'Signature name qual'type))
    = name ++ " :: " ++ show qual'type
  show (Data'Decl name params constrs)
    = "data " ++ name ++ " " ++ unwords params ++ " = " ++ intercalate " | " (map show constrs)
  show (Type'Alias name type')
    = "type " ++ name ++ " = " ++ show type'
  show (Fixity fix prec name)
    = show fix ++ " " ++ show prec ++ " " ++ name
  show (Class name param supers type'decls)
    = "TODO: implement class"
  show (Instance qual'pred decls)
    = "TODO: implement instance"


data Constr'Decl
  = Con'Decl Name [Type]
  | Con'Record'Decl Name [(Name, Type)]
  deriving (Eq)


instance Show Constr'Decl where
  show (Con'Decl name types)
    = name ++ " " ++ unwords (map show types)
  show (Con'Record'Decl name pairs)
    = name ++ " { " ++ intercalate ", " (map (\ (name, type') -> name ++ " :: " ++ show type') pairs) ++ " }"
