{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Analysis.TypeSystem.Solver.Bind where


import qualified Data.Map.Strict as Map
import Control.Monad.Except


import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Solver.Solve
import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Occurs


class Bind k a where
  bind :: k -> a -> Solve (Subst k a)


instance Bind T'V Type where
  bind var@(T'V name _) type'
    | T'Var var == type'        = return empty'subst
    | name `occurs'in` type'    = throwError $ Infinite'Type (T'Var var) type'
    -- TODO: I *SHOULD* consider refactoring `occurs'in` so it takes parametrized argument
    -- in this case I would invoke it with TVar
    --
    -- TODO: I think this is interesting!
    -- I don't think infinite type can have any particular Kind
    -- I think that infinite type doesn't have a proper kind in this system
    -- so some better way of reporting that error would be ideal.
    | otherwise                 = return $ Sub $ Map.singleton var type'


instance Bind String Kind where
  bind varname kind'
    | kind' == K'Var varname     = return empty'subst
    | varname `occurs'in` kind' = throwError $ Infinite'Kind (K'Var varname) kind'
    | otherwise                 = return $ Sub $ Map.singleton varname kind'
