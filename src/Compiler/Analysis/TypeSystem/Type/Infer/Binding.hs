module Compiler.Analysis.TypeSystem.Type.Infer.Binding where


import Control.Monad.Except
import Control.Monad.Reader

import Data.Functor.Identity

import qualified Data.Set as Set

import Data.List


import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Binding

import Compiler.Analysis.TypeSystem.Utils.Infer
import Compiler.Analysis.TypeSystem.Utils.Class

import Compiler.Analysis.TypeSystem.InferenceEnv

import Compiler.Analysis.TypeSystem.Solver
import Compiler.Analysis.TypeSystem.Solver.Substitutable

import Compiler.Analysis.TypeSystem.Type.Infer.Match


{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
{- Returning a [Constraint Type] might not be strictly necessary -}
infer'expl :: Explicit -> Infer ([Predicate], [Constraint Type], [Constraint Kind])
infer'expl (Explicit scheme bg@Bind'Group{ name = name, alternatives = matches }) = do
  (qs :=> t) <- instantiate scheme
  (preds, cs't, cs'k) <- infer'matches matches t
  -- now solve it
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      let qs' = apply subst qs
          t'  = apply subst t
          -- so I need to apply the substitution to the typing context
          -- because that is how it's done in the THIH paper
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
          gs = Set.toList (free'vars t') \\ fs
      let sc' = close'over (qs' :=> t')
          not'entail pred = do
            -- not <$> entail c'env qs' pred -- this should be the same thing, but more succinctly written
            entailed <- entail c'env qs' pred
            return $ not entailed
      let ps' = runIdentity $ runExceptT $ filterM not'entail (apply subst preds)
      case ps' of
        Left err -> throwError err
        Right preds' -> do
          case runIdentity $ runExceptT $ split c'env fs gs preds' of
            Left err -> throwError err
            Right (deferred'preds, retained'preds) -> do
              if scheme /= sc' -- TODO: this line is going to break it as it is
                -- I first need to figure out how to do the comparison correctly, then implement Eq for Scheme manually
              then throwError Signature'Too'General
              else  if not (null retained'preds)
                    then throwError Context'Too'Weak
              else return (deferred'preds, cs't, cs'k)


{- Returning a [Constraint Type] might not be strictly necessary -}
infer'impls :: [Implicit] -> Infer ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
infer'impls implicits = do
  let is = map (\ (Implicit b'g) -> name b'g) implicits
  {-  get only the names of the implicit bindings in the same order -}
      make'fresh'tv = fmap (\ t'name -> T'Var (T'V t'name K'Star)) fresh
  {-  gives a fresh type variable -}
  ts <- mapM (const make'fresh'tv) is
  {-  for each name - create a fresh type variable -}
  let scs = map to'scheme ts
  {-  qualify and quantify the fresh type variables - both empty -}
      assumptions = zip is scs -- NOTE: I will need to put this into the typing context later [1]
  {-  assign each name one type scheme -}
      many'matches = map (\ (Implicit b'g) -> alternatives b'g) implicits
  {-  pick only [Match] from each implicit binding -}
  
  -- [1] now I am going to merge them into the typing context
  -- explanation of the zipWithM part: I need to infer list of matches and I also need to
  -- make sure that once inference for every match is done, constraint unifying the resulting type
  -- with the original fresh type variable will be created and registered
  -- results :: [([Predicate], [Constraint Type], [Constraint Kind])] -- for better reading experience
  results <- merge'into't'env assumptions $ zipWithM infer'matches many'matches ts
  {-  infering each [Match] inside the typing context containing all the assumptions about the types of the implicits
      zipWith is needed because infer'matches expects to be given a type which it unifies with the infered type of all the RHSs
  -}
  let preds = concat [ preds  | (preds, _, _) <- results ]
      cs't  = concat [ cs't   | (_, cs't, _) <- results ]
      cs'k  = concat [ cs'k   | (_, _, cs'k) <- results ]
  
  -- now I can solve the type constraints
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      let preds'  = apply subst preds
          ts'     = apply subst ts -- by now each fresh type variable should be replaced with more specific type, BUT it still will just be Qualified Type
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
          vss = map (Set.toList . free'vars) ts'
          gs = foldr1 union vss \\ fs
      case runIdentity $ runExceptT $ split c'env fs (foldr1 intersect vss) preds' of
        Left err -> throwError err
        Right (deferred'preds, retained'preds) -> do
          if restricted implicits then -- Monomorphism Restriction
            let gs'   = gs \\ Set.toList (free'vars retained'preds)
                scs'  = map (quantify gs' . ([] :=> )) ts' -- NOTE: notice that we are not using close'over - that would quantify over
                -- all the free variables in the type, but because of the monomorphism restriction
                -- we must quantify over only some of them -- QUESTION: Which ones can we quantify over and which ones we can't?
                -- TODO: inspect more later!
            in return (deferred'preds ++ retained'preds, zip is scs', cs't, cs'k)
          else
            let scs'  = map (quantify gs . (retained'preds :=> )) ts' -- qualify each substituted type with retained predicates
            in return (deferred'preds, zip is scs', cs't, cs'k)
            -- Question:  Why do I return the `cs't` even thought I already solved them?
            --            Callers of this function will solve them again and again, does it make sense?


restricted :: [Implicit] -> Bool
restricted implicits = any simple alts
  where
    alts = map (\ (Implicit b'g) -> alternatives b'g) implicits
    simple alts = any (null . patterns) alts
