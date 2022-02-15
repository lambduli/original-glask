module Compiler.Analysis.Syntactic.Types where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Extra


import Compiler.Counter ( Counter(Counter, counter), fresh, real'fresh)

import Compiler.Syntax hiding (Data'Decl, Type'Alias)
import Compiler.Syntax.Term
import Compiler.Syntax.ToAST.Translate


{-  This module has a singular purpose.
    It takes a list of Term'Decl and produces a Map Name Kind.

    The resulting dictionary assigns each type constructor a fresh Kind Variable.
-}


type Fresh a = State Counter a


{-  TODO: Implement the Extract type class for [a], Term'Decl and for Term'Expr.
          I think each time it's going to accept a tuple containing the Counter too.  -}
extract :: [Term'Decl] -> Counter -> (Map.Map Name Kind, Counter)
extract t'decls counter = runState (Map.fromList <$> mapMaybeM collect t'decls) counter


-- NOTE: first two branches have the same body, maybe factor it out into a helper function?
collect :: Term'Decl -> Fresh (Maybe (Name, Kind))
collect (Data'Decl name params t'decls) = do
  fresh'name <- fresh
  let result'kind = K'Var fresh'name -- TODO: this should just be K'Star -- always

  fresh'names'params <- mapM (const fresh) params
  let fresh'kinds = map K'Var fresh'names'params

  -- let kind = foldl K'Arr result'kind fresh'kinds -- plain wrong
  let kind = foldl (flip K'Arr) result'kind fresh'kinds

  return $ Just (name, kind)

collect (Type'Alias name params t'type) = do
  fresh'name <- fresh
  let result'kind = K'Var fresh'name

  fresh'names'params <- mapM (const fresh) params
  let fresh'kinds = map K'Var fresh'names'params

  let kind = foldl K'Arr result'kind fresh'kinds

  return $ Just (name, kind)

collect _
  = return Nothing
