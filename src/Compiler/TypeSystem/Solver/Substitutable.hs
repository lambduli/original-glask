{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Solver.Substitutable where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


import Compiler.Syntax.Name
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Qualified
import Compiler.Syntax.Predicate
import {-# SOURCE #-} Compiler.Syntax.Scheme

import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Solver.Substitution


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

instance Substitutable T'V Type Type where
  apply (Sub s) var@(T'Var t'var)
    = Map.findWithDefault var t'var s
  
  apply _ (T'Con ty'con)
    = T'Con ty'con
  
  apply s (T'App t'left t'right)
    = T'App (apply s t'left) (apply s t'right)
  
  apply s (T'Tuple types)
    = T'Tuple $ map (apply s) types


instance Term T'V Type where
  free'vars type' = case type' of
    T'Var t'var ->
      Set.singleton t'var
    T'Con (T'C name kind') ->
      Set.empty
    T'App left right ->
      free'vars left `Set.union` free'vars right
    T'Tuple ts ->
      foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty ts


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


-- | Substitution on Type Schemes

instance Substitutable T'V Scheme Type where
  apply (Sub s) (For'All varnames type')
    = For'All varnames $ apply s' type'
      where s' = Sub $ foldr Map.delete s varnames


instance Term T'V Scheme where
  free'vars (For'All vars type')
    = free'vars type' `Set.difference` Set.fromList vars


-- | Substitution on Constraints

instance Substitutable k a a => Substitutable k (Constraint a) a where
  apply s (t'l `Unify` t'r)
    = apply s t'l `Unify` apply s t'r


instance Term k a => Term k (Constraint a) where
  free'vars (t'l `Unify` t'r)
    = free'vars t'l `Set.union` free'vars t'r


-- | Substitution on Lists of Substituables

instance Substitutable k a b => Substitutable k [a] b where
  apply = map . apply


instance Term k a => Term k [a] where
  free'vars
    = foldr (Set.union . free'vars) Set.empty


-- | Substitution on Type Context

instance Substitutable T'V Type'Env Type where
  apply subst type'env
    = Map.map
        (apply subst)
        type'env


instance Term T'V Type'Env where
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

instance Substitutable T'V t Type => Substitutable T'V (Qualified t) Type where
  apply subst (preds :=> t)
    = apply subst preds :=> apply subst t


instance (Term T'V t) => Term T'V (Qualified t) where
  free'vars (preds :=> t)
    = free'vars preds `Set.union` free'vars t
  -- TODO: check the paper, is it really an union?
  -- aka - can there be a sensible qualified type, where the predicates contain type variables
  -- which are not present in the the type itself?


-- | Substitution on Predicates

instance Substitutable T'V Predicate Type where
  apply subst (Is'In name t)
    = Is'In name (apply subst t)


instance Term T'V Predicate where
  free'vars (Is'In name t)
    = free'vars t


-- | Kind Substitution on the Type Context

instance Substitutable Name Type'Env Kind where
  apply subst type'env
    = Map.map (apply subst) type'env


instance Substitutable Name Scheme Kind where
  apply subst (For'All varnames type')
    = For'All (apply subst varnames) (apply subst type')
  {-  NOTE: Unlike the case of Type Substitution on Scheme, Kind Substitution does not need to be stripped of the substitutions of close-over variables.  -}


instance Substitutable Name T'V Kind where
  apply subst (T'V name kind)
    = T'V name $ apply subst kind


instance Substitutable Name T'C Kind where
  apply subst (T'C name kind)
    = T'C name $ apply subst kind


instance Substitutable Name t Kind => Substitutable Name (Qualified t) Kind where
  apply subst (preds :=> t)
    = apply subst preds :=> apply subst t


instance Substitutable Name Type Kind where
  apply subst (T'Var t'var)
    = T'Var $ apply subst t'var
  
  apply subst (T'Con ty'con)
    = T'Con $ apply subst ty'con
  
  apply subst (T'App t'left t'right)
    = T'App (apply subst t'left) (apply subst t'right)
  
  apply subst (T'Tuple types)
    = T'Tuple $ map (apply subst) types


instance Substitutable Name Predicate Kind where
  apply subst (Is'In name t)
    = Is'In name (apply subst t)
