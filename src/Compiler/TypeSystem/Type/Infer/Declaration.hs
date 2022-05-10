module Compiler.TypeSystem.Type.Infer.Declaration where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Except ( runExceptT, MonadError(throwError) )
import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.Trans.Reader ( asks, local )
import Data.Functor.Identity ( Identity(runIdentity) )
-- import Control.Monad.State ( MonadState(get) )


import Compiler.Counter ( fresh )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Declaration ( Declaration(..) )
import Compiler.Syntax.BindGroup ( Bind'Group(..) )
import Compiler.Syntax.Match ( Match(..) )
import Compiler.Syntax.Pattern ( Pattern(P'Var) )
import Compiler.Syntax.Expression ( Expression(..) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Predicate ( Predicate (Is'In) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type(..), T'V' (T'V'), M'V (Tau) )
import Compiler.Syntax.Signature ( Signature(T'Signature) )
import Compiler.Syntax.Qualified ( Qualified(..) )
import qualified Compiler.Syntax.Placeholder as Placeholder

import Compiler.TypeSystem.Infer ( Infer, Type'Check, get'constraints )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )
import Compiler.TypeSystem.Assumption ( Assumption )
import Compiler.TypeSystem.BindSection ( Bind'Section )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Implicit(Implicit), Method(Method) )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..) )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Kind.Infer.TypeSection ( infer'annotated, kind'specify'annotated )

import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Semantic.Dependency.Binding as Bindings


import Compiler.TypeSystem.Error ( Error(..) )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Type.Infer.BindSection ( infer'bind'section )
import Compiler.TypeSystem.Utils.Class ( reduce )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term (free'vars) )
import Compiler.TypeSystem.Utils.Infer ( default'subst, add'dicts, lookup'dict, lookup'instance )
import Compiler.TypeSystem.Solver.Composable ( Composable(merge) )


import Debug.Trace

{-  TODO: I inteded to use this function to infer list of declarations like `let` blocks.
          I think it would be best to utilize the infrastructure I already have - the one from the THIH.
          This function might be used just to utilize it.
 -}

infer'decls :: [Declaration] -> Type'Check ([Declaration], [Predicate], [Assumption Sigma'Type])
infer'decls decls = do
  let (explicits, implicits) = to'bind'section decls

  kind'subst <- asks kind'substitution

  let {-  !!! Bude potreba substituovat kindy do tech explicitu abych jim plne specifikoval kinds. !!!  -}
      substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) explicits

  new'expls <- kind'specify'annotated substituted'explicits

  {- I am honestly not sure I understand it right - but it seems that it doesn't really matter if I fully generalize here,
    or later for the whole top level binding. Both versions seem to fail for the same not-well-formed programs.
    Only thing that comes to mind - maybe it could be somehow useful to fail sooner and at the place where the error actually is.
    For now, I am going to generalize - solve constraints and try to reduce context and see where it goes from here.
  -}
  -- infer'bind'section (new'expls, implicits)
  (bg', preds, assums) <- infer'types (new'expls, implicits)
  return (from'bind'section bg', preds, assums)
  -- the problem here is - I have new bind section (with placeholders inserted)
  -- but I need [Declaration] not Bind'Section


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


from'bind'section :: Bind'Section -> [Declaration]
from'bind'section (explicits, implicits) = decls
  where
    -- split explicits to binding and annotation
    -- Binding (Bind'Group { name :: Name, alternatives :: [Match] })
    -- Signature (T'Signature Name Sigma'Type)

    ann'bs = map (\ (Explicit sigma b@Bind'Group{name = n}) -> (T'Signature n sigma, b)) explicits
    (signatures, b'groups'e) = unzip ann'bs
    b'groups'i = map (\ (Implicit b) -> b) $ concat implicits

    signs = map Signature signatures
    binds = map Binding $ b'groups'e ++ b'groups'i


    decls = signs ++ binds





-- TOP LEVEL function
-- infer'section :: Bind'Section -> Type'Check ([Predicate], [Assumption Sigma'Type], [Constraint Type])
-- infer'section b'section@(explicits, implicits) = do
--   kind'subst <- asks kind'substitution

--   let {-  !!! Bude potreba substituovat kindy do tech explicitu abych jim plne specifikoval kinds. !!!  -}
--       substituted'explicits = map (\ (Explicit sigma b'g) -> Explicit (apply kind'subst sigma) b'g) explicits

--   new'expls <- infer'annotated substituted'explicits

--   infer'bind'section (new'expls, implicits)
--   -- infer'types b'section


{-  So the question is - do I use this function? More specificaly - do I want to solve the constraints and reduce all the remaining
    Predicates?
    If I do the context reduction, I probably can't have local bindings with types that "forget" some of the Constraints.
    That might be a good thing, it might be a bad thing - I am not entirely sure right now.
-}
infer'types :: Bind'Section -> Type'Check (Bind'Section, [Predicate], [Assumption Sigma'Type])
infer'types bg = do
  (bg', preds, assumptions) <- infer'bind'section bg

  let properly'close :: (Name, Sigma'Type) -> (Name, Sigma'Type)
      properly'close (name, sigma@(T'Forall tvs qual'type)) =
        let mapping = map (\ (T'V' name kind) -> (Tau name kind, T'Var' (T'V' name kind)) ) tvs
            sub = Sub $ Map.fromList mapping
            sigma' = apply sub sigma
        in (name, sigma')
      properly'close _ = error "unexpected: assumption from local declaration is not forall"


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
  
  cs't <- get'constraints
  case run'solve cs't :: Either Error (Subst M'V Type) of
    Left err -> throwError err
    Right subst -> do
      Infer'Env{ type'env = t'env, class'env = c'env } <- ask
      -- I am applying the substitution to the new version of bind group with placeholders
      -- the next step should be eliminating the placeholders
      -- that is - add extra parameters to functions which are qualified
      -- TODO: implement

      -- let specified = apply subst bg'
      -- I think I might not need to apply the substitution to the AST
      -- instead i pass the substitution to the eliminate function
      -- this one then only looks for the placeholders and applies the substitution to the type in the placeholder
      -- that way I don't need another Substitutable instance for Expression and other stuff, that would be unwieldy

      -- should the function eliminate put the extra dictionary arguments?
      -- probably
      -- so I need a fully substituted typing context/assumptions about all the bindings now
      let assumptions' = apply subst assumptions
          oo = trace ("{ \n... declarations with placeholders: " ++ show bg' ++ "\n and assumptions so far: " ++ show assumptions') assumptions'
      eliminated <- eliminate subst oo bg'
      let aa = trace ("\n ---- and after elimination: " ++ show eliminated) eliminated
      return (aa, preds, map properly'close assumptions')
      -- TODO: NOTE
      --
      --  Tady je takova otazka - v modulu Program ve funkci infer'types
      --  musim provest jeste transformaci tech assumptions, protoze po provedeni substituce uz by sice mely bejt kompletni, ale muze to byt tak,
      --  ze uvnitr jejich sigma typu se bude vyskytovat nejaka promenna jako "meta" ale v tenhle moment uz je kvantifikovana pomoci forall
      --  takze by mela bejt prevedena na T'Var
      --  proto to pak jeste pro jistotu projedu a opatrne zmenim meta promenne na normalni vazane promenne
      --  ale jenom ty, ktere jsou uz kvantifikovane, proto vychazim ze seznamu tvs ve forall
      --  zadne dalsi free meta promenne se ani nedotknu
      --



      -- let rs = runIdentity $ runExceptT $ reduce c'env (apply subst preds)
      -- case rs of
      --   Left err -> throwError err
      --   Right rs' -> do
      --     case runIdentity $ runExceptT $ default'subst c'env [] rs' of
      --       Left err -> throwError err
      --       Right s' -> do
      --         case runIdentity $ runExceptT (s' `merge` subst) of
      --           Left err -> throwError err
      --           Right subst' -> do
      --             {-  QUESTION: Shouldn't I somehow check that the defaulting substitution effectivelly eliminates all the predicates?  -}
      --             {-            Or is it OK if there are some predicates which bubble-up to FROM the top level declarations?            -}
      --             {-  What I meant was - if they are not eliminated by the defaulting substitution, they are effectively unsolved right?
      --                 But maybe the function default'subst fails if it can't eliminate all of them. I think that's the case and the reason
      --                 why it's OK. -}

      --             -- NOTE:  Just testing what happens if I apply the substitution to the method annotations
      --             --        It shoulnd't do any harm. It also shouldn't really have any effect.
      --             --        The type scheme given by the programmer should not change
      --             -- let ms = map (\ (n, q't) -> (n, close'over q't)) m'anns
      --             -- return (apply subst' $ Map.fromList $ assumptions ++ ms, cs'k ++ cs'k')

      --             return (preds, apply subst' assumptions, cs't)


eliminate :: Subst M'V Type -> [(Name, Sigma'Type)] -> Bind'Section -> Type'Check Bind'Section
eliminate subst assumptions (explicits, implicitss) = do
  explicits' <- mapM (elim'expl subst assumptions) explicits

  implicits' <- mapM (mapM (elim'impl subst assumptions)) implicitss
  
  return (explicits', implicits')

-- now I know types for both actually, difference, of course is, the implicitly typed
-- might contain types from upper scope so their types are not complete
-- I should iterate all explicits, and then implicits
-- and when eliminating their Bind'Groups I need to register the extra dictionary parameter into the environment
-- I also need to add that extra parameter for the dictionary
-- to know what dictionary, consider this example:
-- foo :: (Num a, Show b) => a -> b -> a
-- I am not trying to cover super classes now, but suppose that in the future I would want that
-- I very much expect that if the explicitly given typing context can be reduced, the ghc will do it
-- so it might be better to use the type in the typing context right away, rather than the one from explicit
-- right now they are not different, but in the future, it will be simpler to reduce the context and put the correct type in the assumptions,
-- rather than propagate the explicits back to the caller

-- ghc does not reduce it
-- so that is interesting and will have an impact
-- I think it's because of modules, the type must be really what other modules can expect
-- so it can't be different underneath

-- so let's iterate the explicits and try to eliminate them

elim'impl :: Subst M'V Type -> [(Name, Sigma'Type)] -> Implicit -> Type'Check Implicit
elim'impl subst assumptions (Implicit Bind'Group{ name = n, alternatives = alts }) = do
  sigma <- case lookup n assumptions of
            Nothing -> throwError $ Unexpected ("Could not find type for '" ++ n ++ "' in typing assumptions during placeholder elimination")
            Just sigma -> return sigma
  alts' <- mapM (elim'match subst assumptions sigma) alts
  return $ Implicit Bind'Group{ name = n, alternatives = alts' }

elim'expl :: Subst M'V Type -> [(Name, Sigma'Type)] -> Explicit -> Type'Check Explicit
elim'expl subst assumptions (Explicit sigma Bind'Group{ name = n, alternatives = alts }) = do
  alts' <- mapM (elim'match subst assumptions sigma) alts
  return $ Explicit sigma (Bind'Group{ name = n, alternatives = alts' })
-- the point is, alts is a list of Matches
-- so I need to eliminate that too

elim'match :: Subst M'V Type -> [(Name, Sigma'Type)] -> Sigma'Type -> Match -> Type'Check Match
elim'match subst assumptions sigma Match{ patterns = patterns, rhs = rhs } = do
-- I think here is the place for the extra dictionary argument depending on the type
-- so if in the type context there is something like
-- Num a, Show b
-- I know I need to create an extra dictionary parameter for `Num` and type `a` and so on
-- that should be enough at this point
-- because if its the part of the context, it leads to extra dict argument/param
-- I need to produce this ((Name, Type), Name)
-- the tuple is (Class Name, Type) and the right Name is newly introduced name for the parameter
-- instance'env is the name of the slot in the env
  let T'Forall _ (context :=> _) = sigma

  dicts <- mapM new'name context
  -- now I need to make sure they will NOT collide with anything in the scope
  -- so I will do a little dirty trick, prefix them with something that is illegal in the surface language
  -- like "d-"
  -- maybe I could give them names according the class and type variable name?
  -- like "Num-a"
  -- thing is, the type will not always be a variable, this is also valid: (Show (Maybe a), Num (Either a Int)) => ...
  -- so probably not

  -- next I need to add them to the beginning of the pattern list
  let pats = map (\ (_, n) -> P'Var n) dicts
  let patterns' = pats ++ patterns

  -- now I need to eliminate the rhs with the dicts in scope

  rhs' <- add'dicts dicts $ elim'expr assumptions subst rhs

  return Match{ patterns = patterns', rhs = rhs' }

new'name :: Predicate -> Type'Check ((Name, Type), Name)
new'name (Is'In cl'name ty) = do
  name <- fresh
  let param'name = "d-" ++ cl'name ++ "-" ++ name
  let ty'const = get'ty'const ty -- for instances like:
  -- instance Foo (Either a b)
  -- I only care about the Either part, assuming that I will never really care about the `a b` part, when sesarching for the correct dictionary

  return ((cl'name, ty'const), param'name)

elim'expr :: [(Name, Sigma'Type)] -> Subst M'V Type -> Expression -> Type'Check Expression
elim'expr assumps subst v@(Var name) = do
  return v

elim'expr assumps subst c@(Const name) = do
  return c

elim'expr assumps subst o@(Op name) = do
  return o

elim'expr assumps subst l@(Lit lit) = do
  return l

elim'expr assumps subst (Abs pattern body) = do
  -- TODO: here will be some work, I think I can ignore pattern, but need to eliminate body
  body' <- elim'expr assumps subst body
  return (Abs pattern body')

elim'expr assumps subst (App left right) = do
  left' <- elim'expr assumps subst left
  right' <- elim'expr assumps subst right
  return (App left' right')

elim'expr assumps subst (Infix'App left op right) = do
  left' <- elim'expr assumps subst left
  op' <- elim'expr assumps subst op
  right' <- elim'expr assumps subst right
  return (Infix'App left' op' right')

elim'expr assumps subst (Tuple exprs) = do
  exprs' <- mapM (elim'expr assumps subst) exprs
  return (Tuple exprs')

elim'expr assumps subst (If cond then' else') = do
  cond' <- elim'expr assumps subst cond
  then'' <- elim'expr assumps subst then'
  else'' <- elim'expr assumps subst else'
  return (If cond' then'' else'')

elim'expr assumps subst (Let decls expr) = do
  decls' <- mapM (elim'decl assumps subst) decls
  expr' <- elim'expr assumps subst expr
  return (Let decls' expr')

elim'expr assumps subst (Ann expr sigma) = do
  expr' <- elim'expr assumps subst expr
  return (Ann expr' sigma)

elim'expr assumps subst (Case motive matches) = do
  motive' <- elim'expr assumps subst motive
  matches' <- mapM (elim'match' assumps subst) matches
  return (Case motive' matches')

elim'expr assumps subst h@(Hole name) = do
  return h

elim'expr assumps subst p@(Placeholder (Placeholder.Dictionary name ty)) = do
  let type' = apply subst ty
  -- this is from application of variable to possibly many such placeholders
  -- this placeholder resolves to either:
      -- specific instance from the global scope
      -- dictionary parameter - looked up in the instance'env
  case type' of
    T'Var' t' -> do
      -- if it's a rigid type variable, then there must be a dictionary passed as an argument which I can use
      -- I should be able to look it up
      param'name <- lookup'dict (name, type')
      return $ Var param'name
    T'Meta m'v -> do
      -- this should mean that this type variable is from the outer scope, leaving the placeholder as it is
      return p
    T'Con t'c -> do
      -- it is some specific type, and I can assume that there is an instance for this type (for that class), so I look it up in the global scope
      -- no need to look it up in global scope, it is in instance'env together with local dictionary variables
      param'name <- lookup'dict (name, type')
      return $ Var param'name
    T'Tuple tys -> do
      -- I think - same as above?
      undefined
    T'App ty' ty'' -> do
      -- this might be a bit more complicated, the constructor might be known, and it might not
      -- also, when I finally get to know the constructor like Maybe or Either or [], its instance might have a context, that leads to more work
      -- lets start by getting the type constructor
      -- it might be a meta type variable, in that case, I need to wait longer until this placeholder can be resolved
      let t'constr = get'ty'const ty'
      case t'constr of
        T'Var' t' -> do
          -- very exhaustive discussion was moved to the markdown file on method elaboration
          -- conclusion is, at this point, the typ earguments do not matter,
          -- the fact that the type constructor is a rigid type variable means, I will be given the instance in its complete form
          -- I can just lookup the dictionary variable in the scope
          param'name <- lookup'dict (name, t'constr)
          return $ Var param'name

        T'Meta m'v -> do-- this might resolve to a concrete type, must wait
          -- instead of returning the same thing though, I will update the type in the placeholder, just in case the "properly'closing" substitution
          -- changed something
          return $ Placeholder $ Placeholder.Dictionary name type'

        T'Con t'c -> do -- tady neco udelam (tenhle znamena, ze instance existuje v globalu)
          -- if some of the arguments are flexible variables I will need to wait for it to become a specific thing (or a rigid type variable)
          -- because it is my responsibility to find the correct instance for it, in case the context for the current placeholder requires it
          -- like    instance Eq a => Eq [a]
          -- if the `a` here is just a flexible type variable, I can't really know where to get the `Eq a` instance/dictionary to complete the instance for `Eq []`
          -- on the other hand, if the instance `Eq []` were not qualified with a context, or the context does not contain a predicate for that specific type
          -- the one represented by the flexible type variable, I don't have to wait, and can just resolve the placeholder immediately
          -- but that seems to just be an optimization
          
          -- if I want to be smart about it
          -- I would first find the corresponding instance (not the dictionary, I need to know the context for that instance)
          -- that can be found in the environment
          -- if the type is `[Int]` or `[a]` or `[?t1]` I am always searching `Foo []`, that way it does not matter
          -- in the future, when I have flexible instances I think this will work nicely too
          -- but when I have overlapping instances, I might get a list of instances and will need to figure out the correct one
          inst@(dict'name, context, inst'type) <- lookup'instance (name, t'constr)
          -- if the instance has an empty context -> easy, less work to do
          -- if it does not have an empty context, then what I need is to map the type variables in the context to the type arguments in my current type application
          -- I have the `inst'type` part specifically for that
          -- that part looks exactly like in the instance - so    instance (Eq a, Eq b) => Eq (Either a b)
          -- its the `Either a b` part, what I need is to associate those type variables (or in case of flexible instances - they are going to be a bit more complicated types)
          -- but in any case, I need some association, for when I start going through the instance context, there will be some predicates qualifying over a type variables
          -- (or types in general if I have flexible contexts)
          -- the point is - each predicate (be it a simple one, currently, or more complex ones in the future) maps to a dictionary
          -- dictionary which I need to get somewhere and use it to complete the general instance
          -- here is something that I just thought of - I don't need to care about flexible variables up to this point, actually at all
          -- because each predicate in the context will result in another placeholder passed to some "magic" global dictionary-completing function
          -- I will just put the types here, using the mapping, so if I have something like
          -- the original placeholder calls for <Iq, Triple a (m Int b) ?t1>    where `a, m, and b` are rigid type variables and `?t1` is flexible one
          -- suppose the instance context for Iq for Triple is this     (Iq a, Soo m, Bar b, Foo ?t1)
          -- those type variable names will be different ofcourse, but I did an extra step to simplify and already "substituted" the types I have
          -- into the context
          -- all of thos dictionaries must be passed in this order into the specifying/completing function
          -- that means I will just produce the substitution and apply it to the context
          -- obtain the list of substiututed predicates and map them into placeholders
          -- then construct application
          -- at this point I have the dictionary by its variable name in `dict'name`
          -- so i create something like `complete'triple $dict'name [placeholders]`
          -- then I can call the eliminating function again on all placeholders
          -- some of them might get resolved
          -- some of them will need to wait - those with flexible variables at important places
          -- this is great stuff

          -- maybe I could run the unification?
          -- like unify the instance type from the instance declaration and the thing I currently have
          -- this will tell me which type variables from the instance head are what types and that way I can use them when constructing the instance-context-placeholders
          -- before I can do that though, I need to "instantiate" the instance type, because otherwise rigid type variables won't work
          -- 
          let free'       = Set.toList $ free'vars inst'type :: [T'V']
              -- instantiating substitution
              inst'subst  = Sub $ Map.fromList $  map (\ t'v@(T'V' n k) -> (t'v, T'Meta (Tau n k))) free'
              Is'In _ i'ty = inst'type
              inst'type'  = apply inst'subst i'ty
              context'    = apply inst'subst context

          subst <- case run'solve [type' `Unify` inst'type'] :: Either Error (Subst M'V Type) of
                    Left er -> throwError er
                    Right su -> return su
          -- now I have the substitution and I can apply it to instantiated context
          -- which should give me the context but instead of flexible type variables
          -- I should have the specific types for this placeholder
          let specific'ctxt = apply subst context'
          -- now I should be able to just map the specified context into a list of placeholders
          -- the difference is this: here the leftmost fun expr is the variable which holds a function, taking a list of dictionaries and returning the fully specified dictionary
          -- this code is taken from Expression.hs - if there's any mistake it should be fixed there and also few lines down in this file
          let placeholders  = map (\ (Is'In cl'name ty) -> Placeholder $ Placeholder.Dictionary cl'name ty) specific'ctxt
              application   = foldl App (Var dict'name) placeholders -- this is the function, which takes all the extra instances and finally produces one complete instance
          -- this expression needs to be eliminated, some of those extra dictionaries will be figured out immediately, some of them later
          elim'expr assumps subst application -- and that should be it
          -- I am making an expression like
          -- eq-list <Eq, ?>
          -- where ? stands for some type, type of the content of the list
          -- it might be some specific type -> the instance will be synthetized, it might be a rigid type variable -> it will be taken as extra param
          -- it might be more complicated thing, then it will take some more work, but ultimately, it will be taken care of by the existing implementation at this point
          -- at least I hope

        T'Tuple tys -> throwError $ Unexpected "syntactically illegal - tuple is not a type constructor" -- tuple is not a type constructor
        T'App ty4 ty5 -> throwError $ Unexpected "impossible - type application is not a type constructor" -- this is impossible
        T'Forall t's qual -> throwError $ Unexpected "syntactically illegal - forall is not a type constructor" -- forall is not a type constructor

    T'Forall t's qual -> do
      -- I think this can never happen
      -- foralls can never be part of the unification, so they can never end up in the substitution?
      throwError $ Unexpected "Can not find a dictionary for a forall type - this shouldn't happen, something must be broken."

elim'expr assumps subst (Placeholder (Placeholder.Method name ty cl'name)) = do
  let type' = apply subst ty
  -- this placeholder represents a method to be selected from a dictionary
  -- this means I will need to produce the dictionary too
  -- same as above, it can be some specific one, or the one passed as an argument
  -- the selection function should have the same name as the method

  -- whether I will synthetize or get it as a parameter depends on the `ty` type
  -- if the root of the type is known - I will always synthetize it
  -- that means if its just type constant or if its type application with type constant at the leftmost position - then it can be synthetized
    -- of course in the more complicated case there is a catch - in case there is an instance context, I might need to wait until
  -- WAIT WAIT WAIT
  -- is it right that Method placeholders are actually just a dictionary placeholders in disguise?
  -- because what you do is create an expression like:
  -- `<select-name> <dictionary placeholder for type>`
  -- this means that I can actually produce the dictionary placeholder and use the implementation I already have above right?
  -- I need to verify this in the paper!

  -- so I think partially it is true
  -- if I don't care about optimization (if the instance is to be synthetized, I could pick the method var-name from it directly - assuming all methods have unique names)
  -- but I don't care about that at this point, I will make the selection from the synthetized dictionary instead

  -- so I make the selection variable
  let selector = Var name -- the method's name is actually getter function's name
  -- now I make the placeholder
  let placeholder = Placeholder $ Placeholder.Dictionary cl'name type'
  -- now to make the application
  let app = selector `App` placeholder
  -- now to return the result of elimination of that application
  eliminated <- elim'expr assumps subst app

  let oo = trace ("tracing method placeholder | original: " ++ show ((Placeholder.Method name ty cl'name)) ++ "  |  eliminated: " ++ show eliminated ++ "  | subst: " ++ show subst) eliminated

  return oo
  

elim'expr assumps subst (Placeholder (Placeholder.Recursive name ty)) = do
  let type' = apply subst ty
  -- this is basically normal overloaded variable
  -- it will end up being the variable applied to corresponding number of dictionaries
  -- basically I need to produce the application now
  -- can I eliminate those new placeholders now?
  -- lets have an example:
  -- I call recursive function `member a xs` 
  -- so this leads to instantiating the type of `member` - that is just fresh variable at this point
  -- so instead I place the placeholder and store the type (the fresh variable)
  -- now I can apply the substitution and figure out that the type of member is something like
  -- member :: Eq a => a -> [a] -> Bool
  -- so knowing that, I would translate that into an application of the variable to possibly none or many dictionaries/placeholders
  -- so those placeholders, do I know what types they should be?
  -- maybe its bit more complicated, but if it required some dictionary being passed as parameter, the type system has already figured out (right?)
    -- and that means the parameter will be there

  -- but I need to know the type of that variable
  -- and I am not passing assumptions, instead I should be able to use the substitution to make the `ty` into the correct type
  -- but that type will not be qualified
  -- so I actually need assumptions too, just because of this thing
  -- so first I get the type of the recursive identifier
  real'type <- case lookup name assumps of
                Nothing -> throwError $ Unexpected ("Name for the recursive placeholder '" ++ name ++ "' not found - internal error")
                Just ty' -> return ty'
  let T'Forall _ (context :=> _) = real'type
  -- now I know the context of the recursive identifier
  -- for each predicate in there, I will need to place one placeholder for a dictionary
  -- and compose an application of the identifier to all of them
  -- that I can eliminate again and return the result

  -- this piece of code is taken from the Expression.hs module for inference
  let placeholders  = map (\ (Is'In cl'name ty) -> Placeholder $ Placeholder.Dictionary cl'name ty) context
      application   = foldl App (Var name) placeholders
  -- I think the type in the placeholder might not even matter
  -- weird
  elim'expr assumps subst application

-- when this is called, I don't care about adding dictionaries to those declarations
-- that has already been done, at this point, I might specify some type variables within though
-- so I only care about the contexts of the declarations and the rhs of the let
elim'decl :: [(Name, Sigma'Type)] -> Subst M'V Type -> Declaration -> Type'Check Declaration
elim'decl assumps subst (Binding Bind'Group{ name = n, alternatives = matches }) = do
  matches' <- mapM (elim'match' assumps subst) matches
  return $ Binding $ Bind'Group{ name = n, alternatives = matches }

elim'decl assumps subst s@(Signature _) = do
  return s

elim'decl assumps subst d@(Data'Decl _) = do
  return d

elim'decl assumps subst a@(Type'Alias _ _ _) = do
  return a

elim'decl assumps subst f@(Fixity _ _ _ _) = do
  return f

elim'decl assumps subst c@(Class'Decl _) = do
  return c

elim'decl assumps subst i@(Instance _ _) = do
  return i

-- this is for case expressions
elim'match' :: [(Name, Sigma'Type)] -> Subst M'V Type -> Match -> Type'Check Match
elim'match' assumps subst m@Match{ rhs = rhs } = do
  rhs' <- elim'expr assumps subst rhs
  return m{ rhs = rhs' }


get'ty'const :: Type -> Type
get'ty'const ty
  = case ty of
    T'Var' t' -> ty
    T'Meta m'v -> ty
    T'Con t'c -> ty
    T'Tuple tys -> error "tuples are not supported yet" -- and they hardly will be on the left of type application so ...
    T'App ty' ty2 -> get'ty'const ty'
    T'Forall t's qual -> error "syntactically illegal" -- forall is not a type constructor


eliminate'methods :: Subst M'V Type -> [(Name, Sigma'Type)] -> [Method] -> Type'Check [Method]
eliminate'methods subst assumptions methods = do
  let text = "\n+++++++ assumptions: " ++ show assumptions
          ++ "\n        methods: " ++ show methods
          ++ "\n"
      oo = trace text assumptions
  methods' <- mapM (elim'method subst oo) methods
  let message = "\n------ methods afeter: " ++ show methods'
      ee = trace message methods'

  return ee


elim'method :: Subst M'V Type -> [(Name, Sigma'Type)] -> Method -> Type'Check Method
elim'method subst assums (Method sigma Bind'Group{ name = n, alternatives = alts } c'name d'name) = do
  alts' <- mapM (elim'match subst assums sigma) alts
  return $ Method sigma (Bind'Group{ name = n, alternatives = alts' }) c'name d'name
