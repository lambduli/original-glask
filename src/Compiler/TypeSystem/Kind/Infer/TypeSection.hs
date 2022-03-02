{-# LANGUAGE TupleSections #-}

module Compiler.TypeSystem.Kind.Infer.TypeSection where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Bifunctor ( second )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Syntax.Declaration ( Data(..), Class(..) )
import Compiler.Syntax.Kind ( Kind (K'Var, K'Star) )
import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(..), T'V(..) )

import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.TypeSection ( Type'Section )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )
import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env, merge'into'constr'env )
import Compiler.TypeSystem.Solver.Substitution ( Subst(Sub) )
import Compiler.TypeSystem.Solver.Substitutable ( Term(free'vars), Substitutable (apply) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Binding ( Explicit(..), Method(..) )

import Compiler.TypeSystem.Kind.Infer.Data ( infer'data )
import Compiler.TypeSystem.Kind.Infer.Class ( infer'class )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'infer'sigma )

import Compiler.TypeSystem.Error ( Error )


{-  This module is a counter-part of the BindSection module in the Type Inference.
    Its responsibility is to take care of Kind Inference for single SCC section.
    Such section may contain both `data` and `type class` declarations.

 -}


{-                                         Kind Assumptions Class Assumptions                   -}
infer'type'section :: Type'Section -> Infer ([(Name, Kind)], [(Name, Kind)], [Constraint Kind])
infer'type'section (data', classes) = do
  let type'assums   = [ (n, k) | Data (T'C n k) _ _ <- data' ]
      constr'assums = [ (n, k) | Class n (T'V _ k) _ _ <- classes ]

  {-            cl'assumps'd    should always be empty []  -}
  (k'assumps'd, cl'assumps'd, k'cs'd) <- merge'into'k'env type'assums (merge'into'constr'env constr'assums ( infer'seq infer'data data' ))
  
  {- k'assumps'c                should always be empty []  -}
  (k'assumps'c, cl'assumps'c, k'cs'c) <- merge'into'k'env type'assums (merge'into'constr'env constr'assums ( infer'seq infer'class classes ))

  case run'solve (k'cs'd ++ k'cs'c) :: Either Error (Subst Name Kind) of
    Left err -> throwError err
    Right subst -> do
      -- co ted?
      -- mel tu substituci aplikovat na vsechny assumps co jsem zatim dostal
      -- pak je musim projit a najit ty kindy, ktery jsou porad promenny
      -- ty promenny musim vzit a vytvorit z nich substituce do *
      -- to by melo perfektne bezpecny i bez vytvareni a znovureseni constraintu
      -- protoze pokud neco muze byt cokoliv
      -- tak to v pohode muze byt *
      let k'a'd'  = map (second (apply subst)) k'assumps'd
          cl'a'd' = map (second (apply subst)) cl'assumps'd
          k'a'c'  = map (second (apply subst)) k'assumps'c
          cl'a'c' = map (second (apply subst)) cl'assumps'c
          
      let defaulting'mapping  = make'default (k'a'd' ++ cl'a'd' ++ k'a'c' ++ cl'a'c')
          defaulting'subst    = Sub $ Map.fromList defaulting'mapping

      let kind'assumps        = apply defaulting'subst (k'a'd' ++ k'a'c')
          class'assumps       = apply defaulting'subst (cl'a'd' ++ cl'a'c')

      {-  NOTE: This is done because I am returning the constraints to top level for it to solve again when all of constraints are collected.
                When that happens - if not done this - the resulting substitution would not be defaulting.
                It would leave some of the Type Constants and Class Constraints having kind variables in them.
                This way the information that they have been defaulted to * is added to the constraints and will be available at the top level.
      -}
      let defaulting'constraints  = map (\ (n, k) -> K'Var n `Unify` k) defaulting'mapping
          constraints             = k'cs'd ++ k'cs'c ++ defaulting'constraints

      return (kind'assumps, class'assumps, constraints)


make'default :: [(Name, Kind)] -> [(Name, Kind)]
make'default assumptions = map (, K'Star) all'kind'variables
  where
    all'kind'variables :: [Name]
    all'kind'variables = Set.toList $ free'vars $ map snd assumptions


infer'seq :: (a -> Infer ([(Name, Kind)], [(Name, Kind)], [Constraint Kind])) -> [a] -> Infer ([(Name, Kind)], [(Name, Kind)], [Constraint Kind])
infer'seq _ [] = return ([], [], [])
infer'seq ti (bs : bss) = do
  (k'assumps, cl'assumps, k'cs) <- ti bs
  (k'assumps', cl'assumps', k'cs') <- merge'into'k'env k'assumps (merge'into'constr'env cl'assumps ( infer'seq ti bss ))
  return (k'assumps ++ k'assumps', cl'assumps ++ cl'assumps', k'cs ++ k'cs')



infer'annotated :: [Explicit] -> Infer [Explicit]
infer'annotated explicits = mapM do'kind'explicit explicits
  where
    do'kind'explicit :: Explicit -> Infer Explicit
    do'kind'explicit (Explicit sigma b'g) = do
      sigma' <- kind'infer'sigma sigma
      return $ Explicit sigma' b'g


infer'methods :: [Method] -> Infer [Method]
infer'methods methods = mapM do'kind'method methods
  where
    do'kind'method :: Method -> Infer Method
    do'kind'method (Method sigma b'g) = do
      sigma' <- kind'infer'sigma sigma
      return $ Method sigma' b'g
