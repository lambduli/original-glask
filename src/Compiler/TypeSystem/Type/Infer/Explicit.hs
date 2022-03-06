module Compiler.TypeSystem.Type.Infer.Explicit where


import Control.Monad.Except ( filterM, runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Functor.Identity ( Identity(runIdentity) )
import qualified Data.Set as Set
import Data.List ( (\\) )


import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name, alternatives) )
import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( sh, T'V, Type )

import Compiler.TypeSystem.Error ( Error(..) )
import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Binding ( Explicit(..) )
import Compiler.TypeSystem.Utils.Infer ( close'over, instantiate, split )
import Compiler.TypeSystem.Utils.Class ( entail )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, class'env) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Infer.Match ( infer'matches )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'type )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'infer'sigma )


{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
{- Returning a [Constraint Type] might not be strictly necessary -}
infer'expl :: Explicit -> Infer ([Predicate], [Constraint Type])
infer'expl (Explicit scheme bg@Bind'Group{ name = name, alternatives = matches }) = do
  scheme' <- kind'infer'sigma scheme -- this is only needed when infer'expl is invoked on local declarations
  
  (qs :=> t) <- instantiate scheme'
  (preds, cs't) <- infer'matches matches t
  -- now solve it
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      let qs' = apply subst qs
          t'  = apply subst t
          -- so I need to apply the substitution to the typing context
          -- because that is how it's done in the THIH paper
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
          gs = Set.toList (free'vars t') \\ fs
      let sc' = close'over (qs' :=> t')
          not'entail pred = do
            -- not <$> entail c'env qs' pred -- this should be the same thing, but more succinctly written
            entailed <- entail c'env qs' pred
            return $ not entailed
      let ps' = runIdentity $ runExceptT $ filterM not'entail (apply subst preds)
      case ps' of
        Left err -> throwError err
        Right preds' -> do
          case runIdentity $ runExceptT $ split c'env fs gs preds' of
            Left err -> throwError err
            Right (deferred'preds, retained'preds) -> do
              -- NOTE: I need to comment this out - this can not happen right here *1
              -- (k, cs'k') <- infer'type t
              b <- scheme' `sh` sc'
              if not b
              {- TODO:  If I want to know exactly what user-denoted type variable in the `scheme` does correspond to some non-variable type, I can use `match` to create a one-way substitution. -}
              then throwError $ Signature'Too'General scheme' sc'
              else  if not (null retained'preds)
                    then throwError Context'Too'Weak
                    else return (deferred'preds, cs't {- , (k `Unify` K'Star) : cs'k ++ cs'k' -}) -- NOTE *1
              -- TODO:  FIX - here the `kind` function is not used safely
              --        the problem is that kind function expects for Type Applications - that the Kind of the Left part will be Kind Arrow
              --        but that's never going the happen for types given from the user (annotations)
              --        those types contain fresh Kind Variables and not Kind Arrows in general
              --        of course - function types will work - since the (->) is builtin type constructor with the correct Kind assigned in the system
              --        but annotations like foo :: m a
              --        would break the system because (m :: ?A) and (a :: ?B)
              --        and the left part definitely does not give Kind Arrow
              --        therefore I am all in favour of the proposition to change the `kind` method to also produce [Constraint Kind]
              --        instead of expecting the left kind to be arrow, it can also work with Kind Variable on the left and simply produce additional constraints to make sure
              --        everything stays sound
              --
              -- NOTE: Fixed - or it seems.
