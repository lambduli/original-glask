module Compiler.Analysis.Syntactic.Types where

import qualified Data.Map.Strict as Map
import Control.Monad.State
import Control.Monad.Extra

import Compiler.Counter


import Compiler.Syntax hiding (Data'Decl, Type'Alias)
import Compiler.Syntax.Term
import Compiler.Syntax.ToAST.Translate


{-  This module has a singular purpose.
    It takes a list of Term'Decl and produces a Map Name Kind.

    The resulting dictionary assigns each type constructor a fresh Kind Variable.
-}


{-  What follows is a temporary solution for an interesting problem.
    For this module to construct the result Map Name Kind
      which then becomes part of the Translate Environment
    I actually need part of the functionality of the Translate.
    Namely - I need the `fresh` and the State part.
    For that reason I will recreate that part of the functionality here and then I can pick the `counter` value from the State monad
    and use it to initialize the Translate's StateT initial value.
    It is kinda weird, I don't think I like it very much, but it is a way to move forward and it should be refactorable in the future when I know better.
 -}



type Fresh a = State Counter a


letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


-- fresh :: Fresh String
fresh :: State Counter String
fresh = do
  Counter{ counter = counter } <- get
  put Counter{ counter = counter + 1 }
  return $ '_' : (letters !! counter) -- NOTE: I am prefixing all the fresh names with `_` because I am worried that the names are going to collide before I refactor this code correctly
-- END OF THE TEMPORARY INFRASTRUCTURE


{-  TODO: Implement the Extract type class for [a], Term'Decl and for Term'Expr.
          I think each time it's going to accept a tuple containing the Counter too.  -}
extract :: [Term'Decl] -> Counter -> (Map.Map Name Kind, Counter)
extract t'decls counter = runState (Map.fromList <$> mapMaybeM collect t'decls) counter
-- TODO: FIX COUNTER - mel bych to brat jako argument zvenku a ne si ho sam nastavit na jednicku


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
