module Compiler.TypeSystem.Kind.Infer.Class where

import Data.Maybe ( mapMaybe )
import Control.Monad.Extra ( concatMapM )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V'(T'V'), Type(T'Forall) )
import Compiler.Syntax.Declaration ( Class(Class), Declaration(..) )
import Compiler.Syntax.Signature ( Signature(T'Signature) )
import Compiler.Syntax.Kind ( Kind(K'Star) )

import Compiler.TypeSystem.Infer ( Infer, Kind'Check, add'constraints )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )

import Compiler.TypeSystem.Kind.Infer.Type ( infer'context, infer'type )

import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env )

import Compiler.TypeSystem.Error ( Error(Unexpected) )


infer'class :: Class -> Kind'Check ([(Name, Kind)], [(Name, Kind)])
infer'class (Class cl'name (T'V' par'name par'kind) supers decls) = do
  let m'anns = mapMaybe only'annotation decls

  {-  QUESTION: Does the merge have any affect? Because if I understand it correctly. The method annotations are closed types, so the class parameter type variable
                is actually generalized by their foralls isn't it? That means, that the type-class-type-parameter variable gets overwritten immediately by each method foraal anyway.
                It's probably not a problem as long as the TO'AST was correct and assigned the same kind-variable to all type variables.
     -}
  merge'into'k'env [(par'name, par'kind)] (mapM_ infer'method m'anns)

  merge'into'k'env [(par'name, par'kind)] (infer'context supers)

  return ([], [(cl'name, par'kind)])


only'annotation :: Declaration -> Maybe (Name, Sigma'Type)
only'annotation (Signature (T'Signature name sigma )) = Just (name, sigma)
only'annotation _ = Nothing


{-  NOTE: This function also needs to merge the assumption regarding the type class variable and its kind.  -}
infer'method :: (Name, Sigma'Type) -> Kind'Check ()
infer'method a@(n, T'Forall tvs (context :=> type')) = do
  let assumptions = map (\ (T'V' name kind) -> (name, kind)) tvs

  merge'into'k'env assumptions (infer'context context)

  merge'into'k'env assumptions (infer'n'unify type')

  return ()

infer'method (n, _) = do
  throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have come across a method '" ++ n ++ "' which does not have explicitly quantified type." 


infer'n'unify :: Type -> Kind'Check ()
infer'n'unify t = do
  k <- infer'type t
  add'constraints [k `Unify` K'Star]
  return ()
