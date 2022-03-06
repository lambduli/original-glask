module Compiler.TypeSystem.Type.Infer.Declaration where

import qualified Data.Map.Strict as Map
import Control.Monad.Except ( runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.Trans.Reader ( asks, local )
import Data.Functor.Identity ( Identity(runIdentity) )
-- import Control.Monad.State ( MonadState(get) )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Declaration ( Declaration )
import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type, T'V )

import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Assumption ( Assumption )
import Compiler.TypeSystem.BindSection ( Bind'Section )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Implicit(Implicit) )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..) )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Kind.Infer.TypeSection ( infer'annotated )

import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Semantic.Dependency.Binding as Bindings


import Compiler.TypeSystem.Error ( Error(..) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Type.Infer.BindSection ( infer'bind'section )
import Compiler.TypeSystem.Utils.Class ( reduce )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )
import Compiler.TypeSystem.Utils.Infer ( default'subst )
import Compiler.TypeSystem.Solver.Composable ( Composable(merge) )




{-  TODO: I inteded to use this function to infer list of declarations like `let` blocks.
          I think it would be best to utilize the infrastructure I already have - the one from the THIH.
          This function might be used just to utilize it.
 -}

infer'decls :: [Declaration] -> Infer ([Predicate], [Assumption Sigma'Type], [Constraint Type])
infer'decls decls = do
  let (explicits, implicits) = to'bind'section decls

  kind'subst <- asks kind'substitution

  let {-  !!! Bude potreba substituovat kindy do tech explicitu abych jim plne specifikoval kinds. !!!  -}
      substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) explicits

  new'expls <- infer'annotated substituted'explicits

  {- I am honestly not sure I understand it right - but it seems that it doesn't really matter if I fully generalize here,
    or later for the whole top level binding. Both versions seem to fail for the same not-well-formed programs.
    Only thing that comes to mind - maybe it could be somehow useful to fail sooner and at the place where the error actually is.
    For now, I am going to generalize - solve constraints and try to reduce context and see where it goes from here.
  -}
  -- infer'bind'section (new'expls, implicits)
  infer'types (new'expls, implicits)


{- TODO:
  NOW:
  `tiBindGroup` takes the Bind'Groups
  it first puts all the type bindings for the explicitly annotated declarations into the typing context
  then it uses another function to effectively go over the list of implicitly typed declarations and infer
  the whole group with the use of a function `tiImpls`.
  That function does the usual stuff - but there IS an interesting part of it.
  It infers the types, accumulates the Type Constraints and then it solves them.
  It then applies the obtained substitution to mostly everything, to be specific
  it applies it to the typing context (Assumptions) which it then returns.
  This way it is ensured that only the type inference for the small group of implicittly typed bidings
  will get the access to the original assumptions stating that each variable declared is of any type at all
  (via a fresh type variable not closed under its type scheme).
  That unprotected type variable is then replaced in the process of the substitution with the infered type.

  When all un-annotated bindings are infered, we take all the assumptions
    (the original ones, the ones obtained by registering all the annotated bindings, and the ones
    obtained by infering the un-annotated bindings), and we infer the types for the explicitly typed
  (annotated) bindings. This only produces a list of Predicates - qualificators.
  I am not sure what for.

  I think it is because after all of the Predicates from the group are collected, we simplify them
  and create a new substitution from that -> then we apply that substitution to the typing context and
  obtain a "better version of the type qualificators".

  I need to figure out whether the simplification (`reduce`) is strictly necessary.

-}


{-  INVARIANT:  The function expects only Binds and Signatures. -}
to'bind'section :: [Declaration] -> Bind'Section
to'bind'section decls = (explicits, implicits)
  where
    annotations   :: Map.Map Name Sigma'Type
    annotations   = Annotations.extract decls

    bindings      :: Map.Map Name Bind'Group
    bindings      = Bindings.extract decls -- NOTE: Myslim, ze tohle jde volat jenom tehdy, kdyz uz jsou vsechny Bind Groups mergnuty do jedne - pokud maji stejne jmeno.

    explicit'map  :: Map.Map Name (Sigma'Type, Bind'Group)
    explicit'map  = Map.intersectionWith (,) annotations bindings

    implicit'map  :: Map.Map Name Bind'Group
    implicit'map  = Map.difference bindings explicit'map

    explicits     = map (uncurry Explicit) $ Map.elems explicit'map

    implicits     = map (map Implicit) $ Bindings.sort $ Map.elems implicit'map


-- TOP LEVEL function
infer'section :: Bind'Section -> Infer ([Predicate], [Assumption Sigma'Type], [Constraint Type])
infer'section b'section@(explicits, implicits) = do
  kind'subst <- asks kind'substitution

  let {-  !!! Bude potreba substituovat kindy do tech explicitu abych jim plne specifikoval kinds. !!!  -}
      substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) explicits

  new'expls <- infer'annotated substituted'explicits

  infer'bind'section (new'expls, implicits)
  -- infer'types b'section


{-  So the question is - do I use this function? More specificaly - do I want to solve the constraints and reduce all the remaining
    Predicates?
    If I do the context reduction, I probably can't have local bindings with types that "forget" some of the Constraints.
    That might be a good thing, it might be a bad thing - I am not entirely sure right now.
-}
infer'types :: Bind'Section -> Infer ([Predicate], [Assumption Sigma'Type], [Constraint Type])
infer'types bg = do
  (preds, assumptions, cs't) <- infer'bind'section bg

  -- Question:  So all of the constraints were already solved in smaller groups of them.
  --            Do I expect some different result from solving them all?
  --            I think I can imagine getting an error now, even thought they were fine in smaller groups.
  --            But if I don't get an error, is the substitution any different then something like a merge of all the smaller substitutions?
  -- TODO:  Investigate. Do some experiments to be able to tell.

  -- Question:  So what exactly are the `preds` needed for?
  --            I can see that I apply the defaulting substitution to them.
  --            So do I need the retained predicates to figure out ambiguities?
  --            If there are no ambiguities, do I get an empty list in `preds`?
  -- TODO:  Investigate. I want to know if it's going to be empty, if I only write declarations which are not ambiguous.
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      let rs = runIdentity $ runExceptT $ reduce c'env (apply subst preds)
      case rs of
        Left err -> throwError err
        Right rs' -> do
          case runIdentity $ runExceptT $ default'subst c'env [] rs' of
            Left err -> throwError err
            Right s' -> do
              case runIdentity $ runExceptT (s' `merge` subst) of
                Left err -> throwError err
                Right subst' -> do
                  {-  QUESTION: Shouldn't I somehow check that the defaulting substitution effectivelly eliminates all the predicates?  -}
                  {-            Or is it OK if there are some predicates which bubble-up to FROM the top level declarations?            -}
                  {-  What I meant was - if they are not eliminated by the defaulting substitution, they are effectively unsolved right?
                      But maybe the function default'subst fails if it can't eliminate all of them. I think that's the case and the reason
                      why it's OK. -}

                  -- NOTE:  Just testing what happens if I apply the substitution to the method annotations
                  --        It shoulnd't do any harm. It also shouldn't really have any effect.
                  --        The type scheme given by the programmer should not change
                  -- let ms = map (\ (n, q't) -> (n, close'over q't)) m'anns
                  -- return (apply subst' $ Map.fromList $ assumptions ++ ms, cs'k ++ cs'k')

                  return (preds, apply subst' assumptions, cs't)
