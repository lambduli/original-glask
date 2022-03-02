module Compiler.Analysis.Syntactic.Types where

import qualified Data.Map.Strict as Map
import Control.Monad.State ( runState, State )
import Control.Monad.Extra ( mapMaybeM )


import Compiler.Counter ( Counter(Counter, counter), fresh, real'fresh )

import Compiler.Syntax.Kind ( Kind(K'Arr, K'Var) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Term.Declaration ( Term'Decl(Type'Alias, Data'Decl, Class'Decl) )


{-  This module has a singular purpose.
    It takes a list of Term'Decl and produces a Map Name Kind.

    The resulting dictionary assigns each type constructor a fresh Kind Variable.
-}


type Fresh a = State Counter a


{-  TODO: Implement the Extract type class for [a], Term'Decl and for Term'Expr.
          I think each time it's going to accept a tuple containing the Counter too.  -}
extract :: [Term'Decl] -> Counter -> ((Map.Map Name Kind, Map.Map Name Kind), Counter)
extract t'decls counter = runState (collect t'decls) counter



collect :: [Term'Decl] -> Fresh (Map.Map Name Kind, Map.Map Name Kind)
collect t'decls = do
  k'ctxt <- Map.fromList <$> mapMaybeM collect'types t'decls
  cl'ctxt <- Map.fromList <$> mapMaybeM collect'classes t'decls

  return (k'ctxt, cl'ctxt)
  

-- NOTE: first two branches have the same body, maybe factor it out into a helper function?
collect'types :: Term'Decl -> Fresh (Maybe (Name, Kind))
collect'types (Data'Decl name params _) = do
  fresh'name <- fresh
  let result'kind = K'Var fresh'name -- TODO: this should just be K'Star -- always

  fresh'names'params <- mapM (const fresh) params
  let fresh'kinds = map K'Var fresh'names'params

  -- let kind = foldl K'Arr result'kind fresh'kinds -- plain wrong
  let kind = foldl (flip K'Arr) result'kind fresh'kinds

  return $ Just (name, kind)

collect'types (Type'Alias name params _) = do
  fresh'name <- fresh
  let result'kind = K'Var fresh'name

  fresh'names'params <- mapM (const fresh) params
  let fresh'kinds = map K'Var fresh'names'params

  let kind = foldl K'Arr result'kind fresh'kinds

  return $ Just (name, kind)

collect'types _
  = return Nothing


collect'classes :: Term'Decl -> Fresh (Maybe (Name, Kind))
collect'classes (Class'Decl c'name _ _ _) = do
  fresh'name <- fresh
  let class'param'kind = K'Var fresh'name

  return $ Just (c'name, class'param'kind)

collect'classes _
  = return Nothing
