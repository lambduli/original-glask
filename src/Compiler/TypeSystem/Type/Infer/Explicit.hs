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
import {-# SOURCE #-} Compiler.Syntax.Type ( T'V', Type (T'Forall), M'V )

import Compiler.TypeSystem.Error ( Error(..) )
import Compiler.TypeSystem.Infer ( Infer, Type'Check, get'constraints )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Binding ( Explicit(..) )
import Compiler.TypeSystem.Utils.Infer ( instantiate, split', skolemise, close'over' )
import Compiler.TypeSystem.Utils.Class ( entail )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, class'env) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Infer.Match ( infer'matches )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'type )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'infer'sigma, kind'specify )
import Compiler.TypeSystem.Expected ( Expected(Check) )


{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
{- Returning a [Constraint Type] might not be strictly necessary -}
infer'expl :: Explicit -> Type'Check (Explicit, [Predicate])
infer'expl (Explicit scheme bg@Bind'Group{ name = name, alternatives = matches }) = do
  scheme' <- kind'specify scheme -- this is only needed when infer'expl is invoked on local declarations
  
  (skolems, qs, t) <- skolemise scheme'

  -- let message = "{{ tracing infer'expl }}"
  --             ++ "\n| scheme: " ++ show scheme
  --             ++ "\n| scheme': " ++ show scheme'
  --             ++ "\n| skolemised: " ++ show (qs :=> t)
  --     oo = trace message t
  {-  Duvod proc pouzivam skolemise misto instantiate je jednoduchy. Jelikoz jde o typovou anotaci, musim skolemizovat.
      Otazka ale zustava - je skolemizace to jediny, co je treba udelat v tomhle miste jinak? Nebo bych mel zkontrolovat
      vic veci? Najdi si implementaci `infer'expr` pro Ann a porovnej s tim co se deje tam, neco podobnyho by se asi melo dit i tady si myslim.
  -}
  (matches', preds, _) <- infer'matches matches $ Check t
  -- now solve it
  cs't <- get'constraints
  case run'solve cs't :: Either Error (Subst M'V Type) of
    Left err -> throwError err
    Right subst -> do
      let qs' = apply subst qs
          t'  = apply subst t
          -- so I need to apply the substitution to the typing context
          -- because that is how it's done in the THIH paper
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let fs = Set.toList $ free'vars $ apply subst t'env
      {-  NOTE: fs are free type variables within the whole typing context, those might be variables which are types of restricted declarations and will be defaulted
                before they are defaulted though, they can easily be used by other declarations - like this one explicitly typed
                that is the reason why this one must not quantify over it, - simply put - it is not yours to quantify over (and it might even cease to be a type variable later - defaulting)
                one thing I am thinking about - could it happen that some implicit restricted thing would be inferred as Num (for example) from its own definition
                but then someone else uses it in such a way that it implies other type class(es) so then it might not be able to be defaulted?
       -}
          gs = Set.toList (free'vars t') \\ fs
      let sc' = close'over' (qs' :=> t')
          not'entail pred = do
            -- not <$> entail c'env qs' pred -- this should be the same thing, but more succinctly written
            entailed <- entail c'env qs' pred
            return $ not entailed
      let ps' = runIdentity $ runExceptT $ filterM not'entail (apply subst preds)
      case ps' of
        Left err -> throwError err
        Right preds' -> do
          case runIdentity $ runExceptT $ split' c'env fs skolems gs preds' of
            Left err -> throwError err
            Right (deferred'preds, retained'preds) -> do
              -- NOTE: I need to comment this out - this can not happen right here *1
              -- (k, cs'k') <- infer'type t
              -- b <- scheme' `sh` sc'
              if False -- not b
              {- TODO:  If I want to know exactly what user-denoted type variable in the `scheme` does correspond to some non-variable type, I can use `match` to create a one-way substitution. -}
              then throwError $ Signature'Too'General scheme' sc'
              else  if not (null retained'preds)
                    then throwError Context'Too'Weak
                    else
                      let scheme'' = T'Forall skolems (qs :=> t)
                          r = return (Explicit scheme'' bg{ name = name, alternatives = matches' } , deferred'preds {-, cs't -} {- , (k `Unify` K'Star) : cs'k ++ cs'k' -}) -- NOTE *1
                          -- message = "{{ infer'expl }}"
                          --         ++ "\n |  deferred'preds: " ++ show deferred'preds
                          --         ++ "\n |  retained'preds: " ++ show retained'preds
                          --         ++ "\n |  preds': " ++ show preds'
                          --         ++ "\n |  apply subst preds: " ++ show (apply subst preds)
                          --         ++ "\n |  sc': " ++ show sc'
                          --         ++ "\n |  fs: " ++ show fs
                          --         ++ "\n |  gs: " ++ show gs
                          --         ++ "\n |  subst: " ++ show subst
                          --         ++ "\n |  skolems: " ++ show skolems
                          --         ++ "\n |  qs: " ++ show qs
                          --         ++ "\n |  t: " ++ show t
                          --         ++ "\n |  t'env: " ++ show t'env
                          --         ++ "\n |  apply subst t'env: " ++ show (apply subst t'env)
                          -- rr = trace message r
                      in r
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
