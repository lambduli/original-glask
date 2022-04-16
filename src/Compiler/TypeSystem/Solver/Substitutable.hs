{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Solver.Substitutable where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(..), T'V'(..), Type(..), M'V(..) )
import Compiler.Syntax.Kind ( Kind(..) )
import Compiler.Syntax.Qualified ( Qualified(..) )
import Compiler.Syntax.Predicate ( Predicate(..) )

import Compiler.TypeSystem.InferenceEnv ( Kind'Env, Type'Env )
import Compiler.TypeSystem.Constraint ( Constraint(..) )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.ClassEnv ( Class'Env(..) )


--
-- TODO:  Consider renaming it to Substitute?
--        Also consider changing the order of type parameters to (Substitutable k v t)
--

-- | Substituable represents a relation between three types
-- | `k` is a type of the Name in the Subst -- the thing being replaced
-- | `v` is a type of the Value in the Subst -- the thing replacing ^^^
-- | `t` is a type of the thing inside which the substitution takes place
class Substitutable k t v where
  apply :: Subst k v -> t -> t


-- TODO: I am not sure why I need this - please add a commentary once you find out!
-- Well it's because free'vars returns a set of something and that something must be Orderable - simple
-- | Term represents a relation between two types
-- | `k` is a type of the value which represents a free variable
-- | `t` is a type of the value which contains the `k` among possibly many other things
class Ord k => Term k t where
  free'vars :: t -> Set.Set k


-- | Substitution on Types

-- for skolemisation for example
instance Substitutable T'V' Type Type where
  apply (Sub s) var@(T'Var' t'var)
    = Map.findWithDefault var t'var s

  apply (Sub s) var@(T'Meta _)
    = var

  apply _ (T'Con ty'con)
    = T'Con ty'con

  apply s (T'App t'left t'right)
    = T'App (apply s t'left) (apply s t'right)

  apply s (T'Tuple types)
    = T'Tuple $ map (apply s) types

  apply s f1@(T'Forall tvs q't)
  -- I can safely ifnore quantified variables, I am only going to be replacing M'V and those can't be closed under the forall
    = T'Forall tvs $ apply s q't


instance Substitutable M'V Type Type where
  apply (Sub s) var@(T'Var' _)
    = var

  apply (Sub s) meta'var@(T'Meta t'var)
    = Map.findWithDefault meta'var t'var s

  apply _ (T'Con ty'con)
    = T'Con ty'con

  apply s (T'App t'left t'right)
    = T'App (apply s t'left) (apply s t'right)

  apply s (T'Tuple types)
    = T'Tuple $ map (apply s) types

  apply s f1@(T'Forall tvs q't)
  -- I can safely ignore quantified variables, I am only going to be replacing M'V and those can't be closed under the forall
    = T'Forall tvs $ apply s q't


{-  This instance is for finding skolem variables within a Type (and also bound variables)  -}
instance Term T'V' Type where
  free'vars type' = case type' of
    T'Var' t'var ->
      Set.singleton t'var
    T'Meta t'var ->
      Set.empty
    T'Con (T'C name kind') ->
      Set.empty
    T'App left right ->
      free'vars left `Set.union` free'vars right
    T'Tuple ts ->
      foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty ts
    T'Forall tvs (preds :=> type') ->
      let free'in'preds = free'vars preds
          free'in'type  = free'vars type'
      in  Set.difference (free'in'preds `Set.union` free'in'type) (Set.fromList tvs)


{-  This instance is for finding "flexible" meta variables within a Type  -}
instance Term M'V Type where
  free'vars type' = case type' of
    T'Var' t'var ->
      Set.empty
    T'Meta t'var ->
      Set.singleton t'var
    T'Con (T'C name kind') ->
      Set.empty
    T'App left right ->
      free'vars left `Set.union` free'vars right
    T'Tuple ts ->
      foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty ts
    T'Forall tvs (preds :=> type') ->
    -- NOTE: meta variables can't be in the `tvs` list
      let free'in'preds = free'vars preds
          free'in'type  = free'vars type'
      in  free'in'preds `Set.union` free'in'type


-- | Substitution on Kinds

instance Substitutable String Kind Kind where
  apply (Sub s) var@(K'Var varname)
    = Map.findWithDefault var varname s
  
  apply s (left `K'Arr` right)
    = apply s left `K'Arr` apply s right
  
  apply _ K'Star
    = K'Star


instance Term String Kind where
  free'vars K'Star
    = Set.empty
  
  free'vars (K'Arr left right)
    = free'vars left `Set.union` free'vars right 
  
  free'vars (K'Var name)
    = Set.singleton name


-- | Substitution on Constraints

instance Substitutable k a a => Substitutable k (Constraint a) a where
  apply s (l `Unify` r)
    = apply s l `Unify` apply s r

  apply s (l `Match` r)
    = apply s l `Match` apply s r


instance Term k a => Term k (Constraint a) where
  free'vars (l `Unify` r)
    = free'vars l `Set.union` free'vars r

  free'vars (l `Match` r)
    = free'vars l `Set.union` free'vars r


-- | Substitution on Lists of Substituables

instance Substitutable k a b => Substitutable k [a] b where
  apply = map . apply


instance Term k a => Term k [a] where
  free'vars
    = foldr (Set.union . free'vars) Set.empty


-- | Useful when doing kind inference
instance Substitutable k a b => Substitutable k (Name, a) b where
  apply subst (name, a)
    = (name, apply subst a)


instance Term k a => Term k (Name, a) where
  free'vars (_, a)
    = free'vars a


-- | Substitution on Type Context

instance Substitutable M'V Type'Env Type where
  apply subst type'env
    = Map.map
        (apply subst)
        type'env


{-  This might be useful for finding skolems within the type environment  -}
-- instance Term T'V' Type'Env where
--   free'vars type'env
--     = Map.foldr
--         (\ scheme free'set -> free'set `Set.union` free'vars scheme)
--         Set.empty
--         type'env


{-  This is used for finding "flexible" meta variables within the type environment  -}
-- instance Term M'V Type'Env where
--   free'vars type'env
--     = Map.foldr
--         (\ scheme free'set -> free'set `Set.union` free'vars scheme)
--         Set.empty
--         type'env

-- universal
instance Term k Type => Term k Type'Env where
  free'vars type'env
    = Map.foldr
        (\ scheme free'set -> free'set `Set.union` free'vars scheme)
        Set.empty
        type'env


-- | Substitution on Kind Context

instance Substitutable String Kind'Env Kind where
  apply subst kind'env
    = Map.map
        (apply subst)
        kind'env


instance Term String Kind'Env where
  free'vars kind'env
    = Map.foldr
        (\ kind' free'set -> free'set `Set.union` free'vars kind')
        Set.empty
        kind'env


-- | Substitution on Qualified Types

-- This should be universaly useful for skolemisation and for applying meta-var substitution
instance (Substitutable k Predicate v, Substitutable k t v) => Substitutable k (Qualified t) v where
  apply subst (preds :=> t)
    = apply subst preds :=> apply subst t


instance Term T'V' t => Term T'V' (Qualified t) where
  free'vars (preds :=> t)
    = free'vars preds `Set.union` free'vars t
  -- TODO: check the paper, is it really an union?
  -- aka - can there be a sensible qualified type, where the predicates contain type variables
  -- which are not present in the the type itself?


instance (Term M'V t) => Term M'V (Qualified t) where
  free'vars (preds :=> t)
    = free'vars preds `Set.union` free'vars t
  -- TODO: check the paper, is it really an union?
  -- aka - can there be a sensible qualified type, where the predicates contain type variables
  -- which are not present in the the type itself?



-- | Substitution on Predicates

-- for skolems and bounds
instance Substitutable T'V' Predicate Type where
  apply subst (Is'In name t)
    = Is'In name (apply subst t)


-- for meta variables
instance Substitutable M'V Predicate Type where
  apply subst (Is'In name t)
    = Is'In name (apply subst t)


-- for skolems and bounds
instance Term T'V' Predicate where
  free'vars (Is'In name t)
    = free'vars t


-- for meta variables
instance Term M'V Predicate where
  free'vars (Is'In name t)
    = free'vars t


-- | Kind Substitution on the Type Context

instance Substitutable Name Type'Env Kind where
  apply subst type'env
    = Map.map (apply subst) type'env


-- instance Substitutable Name Scheme Kind where
--   apply subst (For'All varnames type')
--     = For'All (apply subst varnames) (apply subst type')
  {-  NOTE: Unlike the case of Type Substitution on Scheme, Kind Substitution does not need to be stripped of the substitutions of close-over variables.  -}


instance Substitutable Name T'V' Kind where
  apply subst (T'V' name kind)
    = T'V' name $ apply subst kind


{-  NOTE: I am not really sure this needs to be implemented. All meta variables should always be created only in either of two ways.
          It will either be introduced by the inference - then the kind is simply *.
          Or it may be created by instantiation - all sigma types, which are allowed to be instantiated should already have been
          fully kind specified before. So the T'Var which will produce a new meta variable should already have fully known kind.  -}
instance Substitutable Name M'V Kind where
  apply subst (Tau name kind)
    = Tau name $ apply subst kind

  apply subst (Sigma name kind)
    = Sigma name $ apply subst kind


instance Substitutable Name T'C Kind where
  apply subst (T'C name kind)
    = T'C name $ apply subst kind


-- BIG TODO: I am not sure whether it's safe to comment it out, it says it is overlapping with the one on the 270 something
-- it seems liek it is - we will have to see
-- instance Substitutable Name t Kind => Substitutable Name (Qualified t) Kind where
--   apply subst (preds :=> t)
--     = apply subst preds :=> apply subst t


instance Substitutable Name Type Kind where
  apply subst (T'Var' t'var)
    = T'Var' $ apply subst t'var

  -- NOTE: As mentioned above - I am not sure this can ever happen.
  -- Maybe there's a relation to the scoped type variables, idk
  apply subst (T'Meta m'var)
    = T'Meta $ apply subst m'var
  
  apply subst (T'Con ty'con)
    = T'Con $ apply subst ty'con
  
  apply subst (T'App t'left t'right)
    = T'App (apply subst t'left) (apply subst t'right)
  
  apply subst (T'Tuple types)
    = T'Tuple $ map (apply subst) types

  apply subst (T'Forall tvs qual't)
    = T'Forall (apply subst tvs) (apply subst qual't)


instance Substitutable Name Predicate Kind where
  apply subst (Is'In name t)
    = Is'In name (apply subst t)


--
--
-- kind substitution for Class'Env - to fully specify all the kinds within the Class'Environment
instance Substitutable Name Class'Env Kind where
  apply subst Class'Env{ classes = classes, defaults = defaults }
    = Class'Env{ classes = applied'classes, defaults = apply subst defaults }
      where
        applied'classes = Map.map (\ (supers, instances) -> (supers, apply subst instances)) classes