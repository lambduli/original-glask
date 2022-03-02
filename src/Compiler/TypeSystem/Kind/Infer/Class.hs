module Compiler.TypeSystem.Kind.Infer.Class where

import Data.Maybe ( mapMaybe )
import Control.Monad.Extra ( concatMapM )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V(T'V), Type(T'Forall) )
import Compiler.Syntax.Declaration ( Class(Class), Declaration(..) )
import Compiler.Syntax.Signature ( Signature(T'Signature) )
import Compiler.Syntax.Kind ( Kind(K'Star) )

import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )

import Compiler.TypeSystem.Kind.Infer.Type ( infer'context, infer'type )

import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env )

import Compiler.TypeSystem.Error ( Error(Unexpected) )


infer'class :: Class -> Infer ([(Name, Kind)], [(Name, Kind)], [Constraint Kind])
infer'class (Class cl'name (T'V par'name par'kind) supers decls) = do
  let m'anns = mapMaybe only'annotation decls

  k'method'constrs <- merge'into'k'env [(par'name, par'kind)] (concatMapM infer'method m'anns)

  k'super'constrs <- merge'into'k'env [(par'name, par'kind)] (infer'context supers)
    
  let constraints = k'method'constrs ++ k'super'constrs 

  return ([], [(cl'name, par'kind)], constraints)


only'annotation :: Declaration -> Maybe (Name, Sigma'Type)
only'annotation (Signature (T'Signature name sigma )) = Just (name, sigma)
only'annotation _ = Nothing


{-  NOTE: This function also needs to merge the assumption regarding the type class variable and its kind.  -}
infer'method :: (Name, Sigma'Type) -> Infer [Constraint Kind]
infer'method a@(n, T'Forall tvs (context :=> type')) = do
  let assumptions = map (\ (T'V name kind) -> (name, kind)) tvs
  
  ctxt'k'cs <- merge'into'k'env assumptions (infer'context context)

  t'k'cs <- merge'into'k'env assumptions (infer'n'unify type')

  return (t'k'cs ++ ctxt'k'cs) 

infer'method (n, _) = do
  throwError $ Unexpected $ "Internal Error: While doing Kind Inference I have come across a method '" ++ n ++ "' which does not have explicitly quantified type." 


infer'n'unify :: Type -> Infer [Constraint Kind]
infer'n'unify t = do
  (k, k'cs) <- infer'type t
  return $ (k `Unify` K'Star) : k'cs
