module Compiler.TypeSystem.InferenceEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'C(T'C), T'V(T'V), Type(..) )
import Compiler.Syntax.Kind ( Kind(K'Star, K'Arr) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )

import Compiler.TypeSystem.Type.Constants ( t'Bool, t'Char, t'Double, t'Int, type'fn )
import Compiler.TypeSystem.Class ( Class )


data Infer'Env = Infer'Env { kind'env :: Kind'Env, type'env :: Type'Env, class'env :: Class'Env, constraint'env :: Constraint'Env }
  deriving (Show)


type Constraint'Env = Map.Map Name Kind -- This environment assigns each type class-name a Kind for its single type parameter


type Kind'Env = Map.Map Name Kind

-- TODO: I think I should move this thing somewhere more top level so that it can be better shared between other modules
init'k'env :: Kind'Env
init'k'env = Map.fromList
  [ ("Bool"   , K'Star) 
  , ("Int"    , K'Star) 
  , ("Double" , K'Star) 
  , ("Char"   , K'Star) 
  , ("Unit"   , K'Star)
  , ("(->)"   , K'Star `K'Arr` (K'Star `K'Arr` K'Star))
  , ("[]"     , K'Star `K'Arr` K'Star) ]
  -- TODO: Questions - do I want to have the List type here?
  -- Or do I define list in the prelude in some (more or less) "hacky" way?


type Type'Env = Map.Map Name Sigma'Type -- Scheme

init't'env :: Type'Env
init't'env = Map.fromList
  [ ("#fst",    T'Forall [T'V "a" K'Star, T'V "b" K'Star]   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "b" K'Star)] `type'fn` T'Var (T'V "a" K'Star)))
  , ("#snd",    T'Forall [T'V "a" K'Star, T'V "b" K'Star]   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "b" K'Star)] `type'fn` T'Var (T'V "b" K'Star)))
  , ("#=",      T'Forall [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#<",      T'Forall [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#>",      T'Forall [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#+",      T'Forall []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#+.",     T'Forall []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#*",      T'Forall []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#*.",     T'Forall []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#-",      T'Forall []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#-.",     T'Forall []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#div",    T'Forall []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#/",      T'Forall []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#show",   T'Forall [T'V "a" K'Star]                   $ [] :=> (T'Var (T'V "a" K'Star) `type'fn` T'App (T'Con (T'C "List" (K'Arr K'Star K'Star))) t'Char)) -- wiring the List type into the compiler
  , ("#debug",  T'Forall [T'V "a" K'Star]                   $ [] :=> (T'Var (T'V "a" K'Star) `type'fn` T'Var (T'V "a" K'Star)))
  ]
-- TODO: revise the list in the future


data Class'Env = Class'Env  { classes :: Map.Map Name Class
                            , defaults :: [Type] }
                  deriving (Show)
