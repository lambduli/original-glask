module Compiler.Syntax.Type where


import Data.List


import Compiler.Syntax.Name
import Compiler.Syntax.Kind


data T'V = T'V Name Kind
  deriving (Eq)


instance Show T'V where
  show (T'V name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


{- NOTE: this is SOLELY because Map TVar _ and it's `Map.union` operation -}
-- Maybe this isn't really necessary, deriving (Ord) may also work
instance Ord T'V where
  (T'V n'l _) <= (T'V n'r _) = n'l <= n'r


data T'C = T'C Name Kind
  deriving (Eq)

instance Show T'C where
  show (T'C name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


data Type
  = T'Var T'V
  | T'Con T'C
  | T'Tuple [Type]
  | T'App Type Type
  deriving (Eq)


{-
For now, I am not including representation for Type Synonyms.
I suppose they should be represented as type declarations.
In the most simple form, something like (Name, [Name], Type).

With that, semantic analysis must make sure that all type synonyms are fully applied.
I can also collect constraints before alias substitution - just to get possibly little better error messages.

Although it would be interesting to lift this restriction same as in Frea and see what exactly is lost,
from type safety standpoint. And how far we can get to the cliff before falling.
 -}


instance Show Type where
  show (T'Var (T'V name kind'))
    = name -- ignoring the kind of the type variable
  show (T'Con (T'C name kind'))
    = "(" ++ name ++ " :: " ++ show kind' ++ ")" -- ignoring the kind of the type constant
  show (T'Tuple types)
    = "(" ++ intercalate ", " (map show types) ++ ")"
  -- show (TyArr left@(TyArr _ _) res'type)
    -- = "(" ++ show left ++ ") -> " ++ show res'type
  -- show (TyArr arg'type res'type)
    -- = show arg'type ++ " -> " ++ show res'type
  show (T'App t'left t'right@(T'App _ _))
    = show t'left ++ " (" ++ show t'right ++ ")"
  show (T'App t'left t'right)
    = show t'left ++ " " ++ show t'right
