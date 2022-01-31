module Compiler.Analysis.TypeSystem.InferenceEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Name
import Compiler.Syntax.Type
import Compiler.Syntax.Kind
import Compiler.Syntax.Qualified
import {-# SOURCE #-} Compiler.Syntax.Scheme

import Compiler.Analysis.TypeSystem.Type.Constants
import Compiler.Analysis.TypeSystem.Class


data Infer'Env = Infer'Env { kind'env :: Kind'Env, type'env :: Type'Env, class'env :: Class'Env }


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


type Type'Env = Map.Map Name Scheme

init't'env :: Type'Env
init't'env = Map.fromList
  [ ("#fst",    For'All [T'V "a" K'Star, T'V "b" K'Star]   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "b" K'Star)] `type'fn` T'Var (T'V "a" K'Star)))
  , ("#snd",    For'All [T'V "a" K'Star, T'V "b" K'Star]   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "b" K'Star)] `type'fn` T'Var (T'V "b" K'Star)))
  , ("#=",      For'All [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#<",      For'All [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#>",      For'All [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#+",      For'All []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#+.",     For'All []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#*",      For'All []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#*.",     For'All []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#-",      For'All []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#-.",     For'All []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#div",    For'All []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#/",      For'All []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#show",   For'All [T'V "a" K'Star]                   $ [] :=> (T'Var (T'V "a" K'Star) `type'fn` T'App (T'Con (T'C "List" (K'Arr K'Star K'Star))) t'Char)) -- wiring the List type into the compiler
  , ("#debug",  For'All [T'V "a" K'Star]                   $ [] :=> (T'Var (T'V "a" K'Star) `type'fn` T'Var (T'V "a" K'Star)))
  ]
-- TODO: revise the list in the future


data Class'Env = Class'Env  { classes :: Map.Map Name Class
                            , defaults :: [Type] }
                  deriving (Show)
