{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.TypeSystem.Solver.Bind where


import qualified Data.Map.Strict as Map
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Syntax.Kind ( Kind(..) )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'V'(..), Type(..), M'V(..) )

import Compiler.TypeSystem.Error ( Error(..) )

import Compiler.TypeSystem.Solver.Solve ( Solve )
import Compiler.TypeSystem.Solver.Substitution ( empty'subst, Subst(..) )
import Compiler.TypeSystem.Solver.Occurs ( Occurs(occurs'in) )
import Compiler.TypeSystem.Solver.InvolvesPoly ( involves'poly )
import Compiler.TypeSystem.Solver.Lift ( Lift(lift) )


class Bind k a where
  bind :: k -> a -> Solve (Subst k a)


instance Bind M'V Type where
  -- bind var@(T'V name _) type'
  --   | T'Var var == type'        = return empty'subst
  --   | name `occurs'in` type'    = throwError $ Infinite'Type (T'Var var) type'
  --   -- TODO: I *SHOULD* consider refactoring `occurs'in` so it takes parametrized argument
  --   -- in this case I would invoke it with TVar
  --   --
  --   -- TODO: I think this is interesting!
  --   -- I don't think infinite type can have any particular Kind
  --   -- I think that infinite type doesn't have a proper kind in this system
  --   -- so some better way of reporting that error would be ideal.

  --   -- NOTE:  I can encode the invariant from the PIART here.
  --   --        Type Variable can never be bound to the poly type.
  --   -- TODO:  Maybe that could be enough. Think about other ways the polytype could be introduced into the unification process too!
  --   | (T'Forall _ _) <- type'   = throwError $ Impredicative var type'
  --   | otherwise                 = return $ Sub $ Map.singleton var type'

  bind var@(Tau name _) type'
    | lift var == type'         = return empty'subst
    | name `occurs'in` type'    = throwError $ Infinite'Type (T'Meta var) type'
    -- NOTE:  I can encode the invariant from the PIART here.
    --        Type Variable can never be bound to the poly type.
    -- TODO:  Maybe that could be enough. Think about other ways the polytype could be introduced into the unification process too!
    | involves'poly type'       = throwError $ Impredicative var type'
    | otherwise                 = return $ Sub $ Map.singleton var type'


instance Bind String Kind where
  bind varname kind'
    | kind' == K'Var varname      = return empty'subst
    | varname `occurs'in` kind'   = throwError $ Infinite'Kind (K'Var varname) kind'
    | otherwise                   = return $ Sub $ Map.singleton varname kind'
