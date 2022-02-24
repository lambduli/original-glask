module Compiler.Syntax.Type where


import Data.List


import Compiler.Syntax.Name
import Compiler.Syntax.Kind
import Compiler.Syntax.Qualified


type Sigma'Type = Type

type Rho'Type = Type

type Tau'Type = Type


data T'V = T'V Name Kind
  -- deriving (Eq)


instance Show T'V where
  show (T'V name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


{- NOTE: this is SOLELY because Map T'V _ and it's `Map.findWithDefault` operation -}
{-  The problem is - the T'V contains a Kind value and it must not be taken into an account
    when looking up the variables in the process of substitution.
    Otherwise it would be thrown off by different Kind Variables representing the same thing.
    Maybe I will figure out a way, how to get rid of this problem and then I should be able to refactor this back to deriving. -}
instance Eq T'V where
  T'V v'l _ == T'V v'r _ = v'l == v'r


{- NOTE: this is SOLELY because Map T'V _ and it's `Map.union` operation -}
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
  | T'Forall [T'V] (Qualified Type)
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
    = name -- ignoring the kind of the type constant
  show (T'Tuple types)
    = "(" ++ intercalate ", " (map show types) ++ ")"
  -- show (TyArr left@(TyArr _ _) res'type)
    -- = "(" ++ show left ++ ") -> " ++ show res'type
  -- show (TyArr arg'type res'type)
    -- = show arg'type ++ " -> " ++ show res'type

  -- (a -> b) -> c ... -> (-> a b) c ... (-> ((-> a) b)) c
  show (T'App (T'App (T'Con (T'C "(->)" _)) t'left@(T'App (T'App (T'Con (T'C "(->)" _)) _) _)) t'right)
    = "(" ++ show t'left ++ ") -> " ++ show t'right
  -- a -> b ... -> a b ... (-> a) b
  show (T'App (T'App (T'Con (T'C "(->)" _)) t'left) t'right)
    = show t'left ++ " -> " ++ show t'right

  show (T'App t'left t'right@(T'App _ _))
    = show t'left ++ " (" ++ show t'right ++ ")"
  show (T'App t'left t'right)
    = show t'left ++ " " ++ show t'right

  show (T'Forall tvs qual'type)
    = "(forall " ++ unwords (map show tvs) ++ " . " ++ show qual'type ++ ")"
