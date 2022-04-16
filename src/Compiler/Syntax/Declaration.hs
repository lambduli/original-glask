module Compiler.Syntax.Declaration where

import Data.List ( intercalate )


import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(..), T'V'(..), Type )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Instance ( Instance )
import Compiler.Syntax.Signature ( Signature(..) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.BindGroup ( Bind'Group )
import Compiler.Syntax.Fixity ( Fixity )
import Compiler.Syntax.Associativity ( Associativity )


data Declaration
  = Binding Bind'Group                        -- id x = x
  -- I may need to add information about whether it is explicitly typed or not

  -- | Annotated String Type Expression        -- id :: a -> a ; id x = x
  -- I think this one actually might not be that usefull
  -- I can use combination of Signature and Binding
  | Signature Signature -- Name (Qualified Type)           -- id :: a -> a
  | Data'Decl Data
  | Type'Alias Name [Name] Type               -- type String = List Char
  | Fixity Fixity Associativity Int Name      -- infix 5 +
  | Class'Decl Class -- Name T'V [Predicate] [Declaration] -- class (Super1 a, ... , SuperN a) ==> Name a where { list of Signatures }
  --    cname parname supers     signatures
  | Instance Instance [Declaration]           -- instance ... where { list of Bindings }
  -- NOTE: I think it's possible that I will need to move the [Declaration] inside the Instance
  -- I don't need them now - for type checking, but for code generation, it might be necessary
  -- to have them together
  deriving (Eq)


instance Show Declaration where
  show (Binding bind'group)
    = "TODO: show bind'group"
    -- = name ++ " = " ++ show expr
  -- show (Annotated name type' expr)
    -- = name ++ " :: " ++ show type' ++ "\n" ++ name ++ " = " ++ show expr
  show (Signature (T'Signature name qual'type))
    = name ++ " :: " ++ show qual'type
  show (Data'Decl data')
    = show data'
  show (Type'Alias name params type')
    = "type " ++ name ++ " = " ++ show type'
  show (Fixity fix assoc prec name)
    = show fix ++ " " ++ show assoc ++ " " ++ show prec ++ " " ++ name
  show (Class'Decl class')
    = show class'
  show (Instance qual'pred decls)
    = "instance " ++ show qual'pred ++ " where " ++ show decls


data Data = Data { type'name :: T'C, type'params :: [T'V'], constructors :: [Constr'Decl] }  -- Data type declaration -- name type'params list'of'consturctors'with'params
  deriving (Eq)


instance Show Data where
  show (Data (T'C name k) params constrs)
    = "data " ++ name ++ " " ++ unwords (map (\ (T'V' n k) -> n) params) ++ " = " ++ intercalate " | " (map show constrs)


data Constr'Decl
  = Con'Decl Name [Type]
  | Con'Record'Decl Name [(Name, Type)]
  deriving (Eq)


instance Show Constr'Decl where
  show (Con'Decl name types)
    = name ++ " " ++ unwords (map show types)
  show (Con'Record'Decl name pairs)
    = name ++ " { " ++ intercalate ", " (map (\ (name, type') -> name ++ " :: " ++ show type') pairs) ++ " }"


data Class = Class { class'name :: Name, class'param :: T'V', class'supers :: [Predicate], class'declarations :: [Declaration] }
  deriving (Eq)


instance Show Class where
  show (Class name (T'V' param kind) supers type'decls)
    = "class " ++ show supers ++ " => " ++ name ++ " (" ++ param ++ " :: " ++ show kind ++ ") where " ++ show type'decls
