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
import Compiler.TypeSystem.Type.Infer.Match ( check'matches )
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
infer'expl e@(Explicit scheme bg@Bind'Group{ name = name, alternatives = matches }) = do
  scheme' <- kind'specify scheme -- this is only needed when infer'expl is invoked on local declarations
  
  (skolems, qs, t) <- skolemise scheme'

  {-  Duvod proc pouzivam skolemise misto instantiate je jednoduchy. Jelikoz jde o typovou anotaci, musim skolemizovat.
      Otazka ale zustava - je skolemizace to jediny, co je treba udelat v tomhle miste jinak? Nebo bych mel zkontrolovat
      vic veci? Najdi si implementaci `infer'expr` pro Ann a porovnej s tim co se deje tam, neco podobnyho by se asi melo dit i tady si myslim.
  -}
  (matches', preds) <- check'matches matches t

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
              {- TODO:  If I want to know exactly what user-denoted type
              variable in the `scheme` does correspond to some non-variable
              type, I can use `match` to create a one-way substitution. -}
              if not (null retained'preds)
              then do
                throwError Context'Too'Weak
              else
                let scheme'' = T'Forall skolems (qs :=> t)
                in return (Explicit scheme'' bg{ name = name, alternatives = matches' }, deferred'preds)
