module Compiler.TypeSystem.Type.Infer.Implicit where


import Control.Monad.Except ( zipWithM, runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )

import Data.Functor.Identity ( Identity(runIdentity) )

import qualified Data.Set as Set

import Data.List ( (\\), intersect, union )


import Compiler.Counter ( fresh )

import Compiler.Syntax.BindGroup ( Bind'Group(name, alternatives) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Match ( Match(patterns) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V'(..), Type(..), M'V(..) )

import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'constraints, get'constraints )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )
import Compiler.TypeSystem.Binding ( Implicit(..) )
import Compiler.TypeSystem.Utils.Infer ( merge'into't'env, quantify, split, to'scheme )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, class'env) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Infer.Match ( infer'matches )
import Compiler.TypeSystem.Expected ( Expected(Infer, Check) )
import Compiler.TypeSystem.Actual ( Actual (Inferred) )


{- Returning a [Constraint Type] might not be strictly necessary -}
infer'impls :: [Implicit] -> Type'Check ([Predicate], [(Name, Sigma'Type)], [Constraint Type])
infer'impls implicits = do
  let is = map (\ (Implicit b'g) -> name b'g) implicits
  {-  get only the names of the implicit bindings in the same order -}
      
      -- BIG TODO: Tohle je takovej pokus - pokud to funguje, tak to je super, jinak musim prijit na to PROC to nejde?
      make'fresh'tv = fmap (\ t'name -> T'Meta (Tau t'name K'Star)) fresh
  {-  gives a fresh type variable -}
  ts <- mapM (const make'fresh'tv) is
  {-  for each name - create a fresh type variable -}
  let scs = map to'scheme ts
  {-  qualify and quantify the fresh type variables - both empty -}
      assumptions = zip is scs -- NOTE: I will need to put this into the typing context later [1]
      -- oo = trace ("{{ tracing infer'impls }}   assumptions " ++ show assumptions) assumptions
      
  {-  assign each name one type scheme -}
      many'matches = map (\ (Implicit b'g) -> alternatives b'g) implicits
  {-  pick only [Match] from each implicit binding -}
  
  -- [1] now I am going to merge them into the typing context
  -- explanation of the zipWithM part: I need to infer list of matches and I also need to
  -- make sure that once inference for every match is done, constraint unifying the resulting type
  -- with the original fresh type variable will be created and registered
  -- results :: [([Predicate], [Constraint Type], [Constraint Kind])] -- for better reading experience
  let infer'ts = map (const Infer) ts
  -- let infer'ts = map Check ts

  results <- merge'into't'env assumptions $ zipWithM infer'matches many'matches infer'ts
  {-  infering each [Match] inside the typing context containing all the assumptions about the types of the implicits
      zipWith is needed because infer'matches expects to be given a type which it unifies with the infered type of all the RHSs
  -}
  let preds = concat [ preds  | (preds, _) <- results ]
      cs't  = concat [ cs't   | (_, _) <- results ]
      types = [ type'  | (_, Inferred type') <- results ]

      con'constraints = zipWith Unify types ts
  
  add'constraints con'constraints
  -- now I can solve the type constraints
  cs't <- get'constraints
  case run'solve cs't {- (cs't ++ con'constraints) -} :: Either Error (Subst M'V Type) of
    Left err -> throwError err
    Right subst -> do
      let preds'  = apply subst preds
          ts'     = apply subst ts -- by now each fresh type variable should be replaced with more specific type, BUT it still will just be Qualified Type
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
          vss = map (Set.toList . free'vars) ts'
          gs = foldr1 union vss \\ fs

      case runIdentity $ runExceptT $ split c'env fs (foldr1 intersect vss) preds' of
        Left err -> do
          -- let message = "{{ tracing }}"
          --             ++ ""
          --             ++ "\n|  constraints: " ++ show cs't
          --             ++ "\n|  assumptions: " ++ show assumptions
          --             ++ "\n|  preds: " ++ show preds
          --             ++ "\n|  preds': " ++ show preds'
          --             ++ "\n|  free vars in t'env: " ++ show fs
          --             ++ "\n|  the other thing: " ++ show (foldr1 intersect vss)
          --             ++ "\n|  ts': " ++ show ts'
          --             ++ "\n|  types: " ++ show types
          --             ++ "\n|  types': " ++ show (apply subst types)
          --             ++ "\n|  all constraints: " ++ show (cs't ++ con'constraints)
          --     ee = trace message err
          throwError err
        Right (deferred'preds, retained'preds) -> do
          if restricted implicits then -- Monomorphism Restriction
            let gs'   = gs \\ Set.toList (free'vars retained'preds)
                scs'  = map (quantify gs' . ([] :=> )) (apply subst types)-- types -- NOTE: notice that we are not using close'over - that would quantify over
                -- all the free variables in the type, but because of the monomorphism restriction
                -- we must quantify over only some of them -- QUESTION: Which ones can we quantify over and which ones we can't?
                -- TODO: inspect more later!
            in return (deferred'preds ++ retained'preds, zip is scs', cs't ++ con'constraints)
          else
            let scs'  = map (quantify gs . (retained'preds :=> )) types -- qualify each substituted type with retained predicates
            in return (deferred'preds, zip is scs', cs't ++ con'constraints)
            -- Question:  Why do I return the `cs't` even thought I already solved them?
            --            Callers of this function will solve them again and again, does it make sense?


restricted :: [Implicit] -> Bool
restricted implicits = any simple alts
  where
    alts = map (\ (Implicit b'g) -> alternatives b'g) implicits
    simple alts = any (null . patterns) alts
