module Compiler.Analysis.TypeSystem.InferenceEnv where


import qualified Data.Map.Strict as Map


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Type.Constants


data Infer'Env = Infer'Env { kind'env :: Kind'Env, type'env :: Type'Env, class'env :: Class'Env }


empty'an'env :: Infer'Env
empty'an'env = Infer'Env init'k'env init't'env init'c'env


-- TODO: important question
-- how exactly is the KindEnv indexed?
-- more specificaly --> is the binding between type variable name in the type scheme
-- and the String here in the KindEnv valid for ever?
-- or is it possible, that some of the "generalize", "instantiate" or similar functions
-- will change the name of the type variable AFTER it was registered into the kind environment with some specific kind?
-- what I am thinking of right now, is the moment, when the inference is done and the substitution is done
-- I think there's a piece of code, which will, roughly speaking, "normalize" all the type schemes in the type env
-- and that would obviously break the bond between the name of the type variable in the type scheme
-- and its kind as associated by the KindEnv
-- yeah, I think this place is exactly the place to look at -- good job me
-- so I think, that if this is in fact true, this would be exactly a reason why to HAVE
-- the kinds in type variables
-- because those bounds can't be broken
-- if everything goes well (inference and solving)
-- substitution will be produced, which will, for each kind variable, assign the kind computed
-- than this substituion can be used on the normalized type schemes even if the names of the type variables no longer correspond
-- good stuff, I think

type Kind'Env = Map.Map Name Kind

init'k'env :: Kind'Env
init'k'env = Map.fromList
  [ ("Bool"   , K'Star) 
  , ("Int"    , K'Star) 
  , ("Double" , K'Star) 
  , ("Char"   , K'Star) 
  , ("Unit"   , K'Star)
  , ("(->)"   , K'Star `K'Arr` (K'Star `K'Arr` K'Star)) ]


type Type'Env = Map.Map Name Scheme

init't'env :: Type'Env
init't'env = Map.fromList
  [ ("#fst",    ForAll [T'V "a" K'Star, T'V "b" K'Star]   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "b" K'Star)] `type'fn` T'Var (T'V "a" K'Star)))
  , ("#snd",    ForAll [T'V "a" K'Star, T'V "b" K'Star]   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "b" K'Star)] `type'fn` T'Var (T'V "b" K'Star)))
  , ("#=",      ForAll [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#<",      ForAll [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#>",      ForAll [T'V "a" K'Star]                   $ [] :=> (T'Tuple [T'Var (T'V "a" K'Star), T'Var (T'V "a" K'Star)] `type'fn` t'Bool))
  , ("#+",      ForAll []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#+.",     ForAll []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#*",      ForAll []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#*.",     ForAll []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#-",      ForAll []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#-.",     ForAll []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#div",    ForAll []                                 $ [] :=> (T'Tuple [t'Int, t'Int] `type'fn` t'Int))
  , ("#/",      ForAll []                                 $ [] :=> (T'Tuple [t'Double, t'Double] `type'fn` t'Double))
  , ("#show",   ForAll [T'V "a" K'Star]                   $ [] :=> (T'Var (T'V "a" K'Star) `type'fn` T'App (T'Con (T'C "List" (K'Arr K'Star K'Star))) t'Char)) -- wiring the List type into the compiler
  , ("#debug",  ForAll [T'V "a" K'Star]                   $ [] :=> (T'Var (T'V "a" K'Star) `type'fn` T'Var (T'V "a" K'Star)))
  ]
-- TODO: revise the list in the future


data Class'Env = Class'Env  { classes :: Map.Map Name Class
                            , defaults :: [Type] }

init'c'env :: Class'Env
init'c'env = undefined
