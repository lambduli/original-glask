module Compiler.TypeSystem.Type.Infer.Implicit where


import Control.Monad.Except ( zipWithM, runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )

import Data.Functor.Identity ( Identity(runIdentity) )

import qualified Data.Set as Set

import Data.List ( (\\), intersect, union )


import Compiler.Counter ( fresh )

import Compiler.Syntax.BindGroup ( Bind'Group(name, alternatives, Bind'Group) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Match ( Match(patterns) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V'(..), Type(..), M'V(..) )
import Compiler.Syntax.Overloaded ( Overloaded(Recursive) )

import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'constraints, get'constraints )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )
import Compiler.TypeSystem.Binding ( Implicit(..) )
import Compiler.TypeSystem.Utils.Infer ( merge'into't'env, quantify, split, to'scheme, overload )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, class'env) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Infer.Match ( infer'matches )
import Compiler.TypeSystem.Expected ( Expected(Infer, Check) )
import Compiler.TypeSystem.Actual ( Actual (Inferred) )


{- Returning a [Constraint Type] might not be strictly necessary -}
infer'impls :: [Implicit] -> Type'Check ([Implicit], [Predicate], [(Name, Sigma'Type)])
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
  {-  assign each name one type scheme -}

  -- TODO: I need to register all the assumptions as Recrusive in the overloaded env
  let recursives = map (\ (n, _) -> (n, Recursive)) assumptions

      many'matches = map (\ (Implicit b'g) -> alternatives b'g) implicits
  {-  pick only [Match] from each implicit binding -}
  
      many'names = map (\ (Implicit b'g) -> name b'g) implicits

  -- NOTE: the overload registers all mutually recursive implicits as recursive, because of placeholders
  results <- overload recursives $ merge'into't'env assumptions $ mapM infer'matches many'matches
  {-  infering each [Match] inside the typing context containing all the assumptions about the types of the implicits
      zipWith is needed because tc'matches expects to be given a type which it unifies with the infered type of all the RHSs
  -}
  let matches' = [ match | (match, _, _) <- results ]
      preds = concat [ preds  | (_, preds, _) <- results ]
      types = [ type'  | (_, _, type') <- results ]

      -- NOTE: I think this might be unnecessary. If I used check'matches instead of infer'matches and push the type variables (not the foralls)
      -- into the matches, they would get unified with the insides on their own, so this line would not be necessary, am I right?
      con'constraints = zipWith Unify types ts

  let implicits' = zipWith (\ n m -> Implicit (Bind'Group{ name = n, alternatives = m })) many'names matches'

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
          throwError err

        Right (deferred'preds, retained'preds) -> do
          if restricted implicits then -- Monomorphism Restriction
            let gs'   = gs \\ Set.toList (free'vars retained'preds)
                scs'  = map (quantify gs' . ([] :=> )) (apply subst types)-- types -- NOTE: notice that we are not using close'over - that would quantify over
                -- all the free variables in the type, but because of the monomorphism restriction
                -- we must quantify over only some of them -- QUESTION: Which ones can we quantify over and which ones we can't?
                -- TODO: inspect more later!
            in return (implicits', deferred'preds ++ retained'preds, zip is scs')
          else
            let scs' = map (quantify gs . (retained'preds :=> )) (apply subst types)-- types -- qualify each substituted type with retained predicates
            in  return (implicits', deferred'preds, zip is scs')


restricted :: [Implicit] -> Bool
restricted implicits = any simple alts
  where
    alts = map (\ (Implicit b'g) -> alternatives b'g) implicits
    simple alts = any (null . patterns) alts
