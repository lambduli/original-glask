{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Type where

import Control.Monad.State ( MonadState )
import qualified Data.Map.Strict as Map

import {-# SOURCE #-} Compiler.Syntax.Qualified ( Qualified )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Counter ( Counter )


type Sigma'Type = Type


type Rho'Type = Type


type Tau'Type = Type


data T'V'  = T'V' Name Kind


instance Show T'V'


{- NOTE: this is SOLELY because Map T'V _ and it's `Map.findWithDefault` operation -}
{-  The problem is - the T'V contains a Kind value and it must not be taken into an account
    when looking up the variables in the process of substitution.
    Otherwise it would be thrown off by different Kind Variables representing the same thing.
    Maybe I will figure out a way, how to get rid of this problem and then I should be able to refactor this back to deriving. -}
instance Eq T'V'


{- NOTE: this is SOLELY because Map T'V _ and it's `Map.union` operation -}
-- Maybe this isn't really necessary, deriving (Ord) may also work
instance Ord T'V'


data M'V  = Sigma Name Kind
          | Tau Name Kind


instance Eq M'V


instance Ord M'V


instance Show M'V


data T'C = T'C Name Kind
  

instance Eq T'C


instance Show T'C


data Type
  = T'Var' T'V'
  | T'Meta M'V
  | T'Con T'C
  | T'Tuple [Type]
  | T'App Type Type
  | T'Forall [T'V'] (Qualified Type)


instance Eq Type


{-
For now, I am not including representation for Type Synonyms.
I suppose they should be represented as type declarations.
In the most simple form, something like (Name, [Name], Type).

With that, semantic analysis must make sure that all type synonyms are fully applied.
I can also collect constraints before alias substitution - just to get possibly little better error messages.

Although it would be interesting to lift this restriction same as in Frea and see what exactly is lost,
from type safety standpoint. And how far we can get to the cliff before falling.
 -}


instance Show Type
