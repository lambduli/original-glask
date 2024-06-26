{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Utils.Infer where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List ( partition, (\\), zipWith )
import Data.Bifunctor ( Bifunctor(second) )

import Control.Monad.Reader ( asks, MonadReader(local, ask) )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Counter ( Counter(Counter, counter), fresh, real'fresh, letters )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.HasKind ( HasKind(kind) )
import Compiler.Syntax.Kind ( Kind (K'Star) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified(..) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V'(..), Type(T'Forall, T'Var', T'Meta, T'Tuple), Rho'Type, Tau'Type, M'V(..) )
import Compiler.Syntax.TFun ( pattern T'Fun )
import Compiler.Syntax.Overloaded ( Overloaded )
import Compiler.Syntax.Match ( Match )

import Compiler.TypeSystem.Error ( Error(Unexpected, Unbound'Var, Unbound'Type'Var) )
import Compiler.TypeSystem.Infer ( Infer, run'infer, Type'Check, Kind'Check, add'constraints, get'constraints )
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, kind'env, constraint'env, overloaded, instance'env, instances, class'env), Type'Env )
import Compiler.TypeSystem.ClassEnv ( Class'Env )
import Compiler.TypeSystem.Ambiguity ( ambiguities, candidates, Ambiguity )
-- import Compiler.TypeSystem.Solver.Solve ( Solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Utils.Class ( reduce, entail )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )
import Compiler.TypeSystem.Type.Constants ( type'fn )


import Compiler.Syntax.Expression ( Expression )
import Compiler.TypeSystem.Expected ( Expected(..) )
import Compiler.TypeSystem.Actual ( Actual(..) )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Expression ( infer'expr )
import Compiler.TypeSystem.Solver ( run'solve )


quantify :: [M'V] -> Qualified Type -> Sigma'Type
quantify meta'vars q'type = sigma
  where
    tvs = map meta'to'tv meta'vars

    meta'to'tv :: M'V -> T'V'
    meta'to'tv (Tau name kind) = T'V' name kind
    -- the question is - is it ever allowed for the meta variable to be a sigma?
    -- I think it should never happen.
    -- so maybe it would be best to panic in such cases with some usefull message

    mapping = zip meta'vars $ map T'Var' tvs
    subst = Sub $ Map.fromList mapping
    
    q'type' = apply subst q'type

    sigma = T'Forall tvs q'type'
-- now, here's the thing
-- because I keep both names and kinds of the parametrized type variables
-- as opposed to only keeping kinds (as does Jones)
-- I don't really need to do anything about vars

-- similarly, because I don't represent bound type variables with distinct data construct (TGen)
-- I don't really need to do any substitution application on the qualified type


{-  INVARIANT:  The argument must not contain any M'Vs  -}
to'scheme :: Type -> Sigma'Type
to'scheme sigma@(T'Forall _ _) = sigma
to'scheme t = T'Forall [] ([] :=> t)


{- Qualifies the Type with no Predicates -}
qualify :: Type -> Qualified Type
qualify t = [] :=> t


overload :: [(String, Overloaded)] -> Type'Check a -> Type'Check a
overload overloads m = do
  let scope e@Infer'Env{ overloaded = over } = e{ overloaded = overloads ++ over}
  local scope m


add'dicts :: [((Name, Type), Name)] -> Type'Check a -> Type'Check a
add'dicts dicts m = do
  let scope e@Infer'Env{ instance'env = dicts' } = e{ instance'env = dicts ++ dicts'}
  local scope m


class With a where
  with :: [(String, a)] -> Type'Check b -> Type'Check b

instance With Type where
  with bindings m = do
    let scope e@Infer'Env{ type'env = t'env } = e{ type'env = Map.fromList bindings `Map.union` t'env}
    local scope m


instance With Kind where
  with bindings m = do
    let scope e@Infer'Env{ kind'env = k'env } = e{ kind'env = Map.fromList bindings `Map.union` k'env }
    local scope m









-- TODO: I really feel like generalizing all the merge'into'... and put'in'... and lookup'...
--        is the best way to go around. Then put them in some shared Utils module and use them
--        across all parts of the pipeline.
merge'into't'env :: [(String, Sigma'Type)] -> Type'Check a -> Type'Check a
merge'into't'env bindings m = do
  let scope e@Infer'Env{ type'env = t'env } = e{ type'env = Map.fromList bindings `Map.union` t'env}
  local scope m


put'in't'env :: (String, Sigma'Type) -> Type'Check a -> Type'Check a
put'in't'env (var, scheme) m = do
  let scope e@Infer'Env{ type'env = t'env } = e{ type'env = Map.insert var scheme (Map.delete var t'env) }
  local scope m


{- TODO NOTE: What about creating some type class for Kind, Type and such
- it would define lookup method and the correct return type would be figured from the type "context"
(as in - call site) -}
lookup't'env :: String -> Expected Type -> Type'Check Sigma'Type
lookup't'env var expected = do
  env <- asks type'env
  case Map.lookup var env of
    Nothing     ->  throwError $ Unbound'Var var expected
    Just scheme ->  return scheme


merge'into'k'env :: [(String, Kind)] -> Kind'Check a -> Kind'Check a
merge'into'k'env bindings m = do
  let scope e@Infer'Env{ kind'env = k'env } = e{ kind'env = Map.fromList bindings `Map.union` k'env }
  local scope m


merge'into'constr'env :: [(Name, Kind)] -> Kind'Check a -> Kind'Check a
merge'into'constr'env bindings m = do
  let scope e@Infer'Env{ constraint'env = constr'env } = e{ constraint'env = Map.fromList bindings `Map.union` constr'env }
  local scope m


put'in'k'env :: (String, Kind) -> Kind'Check a -> Kind'Check a
put'in'k'env (var, kind') m = do
  let scope e@Infer'Env{ kind'env = k'env } = e{ kind'env = Map.insert var kind' (Map.delete var k'env) }
  local scope m


lookup'k'env :: String -> Kind'Check Kind
lookup'k'env var = do
  k'env <- asks kind'env
  case Map.lookup var k'env of
    Nothing     -> throwError $ Unbound'Type'Var var
    Just kind'  -> return kind'


lookup'dict :: (Name, Type) -> Type'Check (Maybe Name)
lookup'dict placeholder = do
  env <- asks instance'env
  return $ lookup placeholder env
  -- case lookup placeholder env of
  --   Nothing -> throwError $ Unexpected ("Can't find dictionary for " ++ show placeholder)
  --   Just name -> return name


lookup'instance :: (Name, Type) -> Type'Check (Name, [Predicate], Predicate)
lookup'instance placeholder = do
  env <- asks instances
  case lookup placeholder env of
    Nothing -> throwError $ Unexpected ("Can't find an instance for " ++ show placeholder ++ " | env: " ++ show env)
    Just instance' -> return instance'

{-  TODO: Here is a BIG TODO - I need to go over the paper THIH and see if I can replace all my
          uses of instantiate and close'over with what Jones does.
          Reason: it seems that Jones always closes and generalizes only over explicitly specified variables.
          That might seem like extra work, but there's moment in the inference where it really comes in handy.
          I think it's related to the inference of explicitly annotated expressions. Not sure.
          So I wonder, if for the sake of being consistent I should replace my somehow autonomous functions with those from THIH.
          I really think it is worth considering.
          Probably also includes `normalize` and `generalize`? I am not sure what they do at this moment. Inspect yourself later.
-}
{- TODO: -}
{-  This function needs to be modified so that it can transform quantified type variables into meta type variables. -}
instantiate :: Sigma'Type -> Type'Check (Qualified Type)
instantiate (T'Forall vars qual'type) = do
  let params = map (\ (T'V' n _) -> n) vars
      kinds  = map (\ (T'V' _ k) -> k) vars

      

      -- f (T'V' n k) = real'fresh params n >>= \ fresh -> return (fresh, k)
  fresh'strs <- mapM (real'fresh params) vars

  let assumptions = zip fresh'strs kinds
      meta'vars = map (\ (name, k) -> T'Meta (Tau name k)) assumptions
  -- NOTE:  So I have fixed the issue where I wrongly instantiated the generic type variables to the Kind *.
  --        I have come to the conclusion that it is OK for all the instantiations of the same Scheme - resp. their generic type variables -
  --        to share the same set of Kind Variables.
  --        My reasoning is following: I don't think it can ever happen that two instantiations of the same Scheme will actually be inferred (and used)
  --        in such a way that their type parameters can be assigned different Kinds.
  --        Example: foo :: m a -> (a -> b) -> m b
  --        I don't think it can ever happen that `m` , `a` , and `b` will be assigned different Kinds for different instantiations.
  --        That includes Phantom Types.
  --        data Phantom valid = Data Int
  --        and then later something like
  --        read :: String -> Phantom a
  --        I don't think it is possible for the `a` to be anything else than `*` in any instantiation.
  --        So the assumptions seems safe and sound.
  let subst = Sub $ Map.fromList $ zip vars meta'vars

  return $ apply subst qual'type

{-  NOTE: Making the function covering/total. Instantiating non forall type is just a lift into Qualified Type  -}
instantiate type' = return $ qualify type'


{-  TODO: Consider removing this function from the codebase.
          I might be able to use `quantify` instead.
          I would need to explicitly specify what variables I want to close over.
          But it might be a better practice.
          As being explicit might give much more information about what is the intent and what variables should be closed over.
-}
close'over :: Qualified Type -> Sigma'Type
close'over = normalize . generalize Map.empty


close'over' :: Qualified Type -> Sigma'Type
close'over' q'type =
  let ftvs :: [T'V']
      ftvs = Set.toList $ free'vars q'type
  in T'Forall ftvs q'type


{-  TODO: In the second stage of the "ExplicitForAll" adoption this function should take into account
          that there might be types like
          forall a . a -> (forall b . b -> a)
          to normalize such types I think it should be done in steps,
          first normalize the outer foralls and then the inner ones
          that way the `a` is going to be renamed correctly even inside the nested forall.
          It even seems like pretty simple operation - but best to be sure it's done correctly.
-}
{- Invariant: The Type argument must be well-formed Sigma Type. -}
{- NOTE:      Currently it doesn't do deep normalization.       -}
normalize :: Sigma'Type -> Sigma'Type
normalize (T'Forall vars q't) = T'Forall vars'norm q't'norm
  where
    to'mapping (tv@(T'V' tv'name kind'), fresh'name) = (tv, T'V' fresh'name kind')
    -- to'mapping (tv, fresh'name) = error $ show tv
    
    -- to'mapping (tv@(T'S tv'name kind'), fresh'name) = (tv, T'V fresh'name kind')


    pairs = zip vars letters
    mapping = map to'mapping pairs
    subst = Sub $ Map.fromList $ map (second T'Var') mapping
    vars'norm = map snd mapping
    q't'norm = apply subst q't

normalize _ = error "Internal Error: 'normalize' was called with a non forall type."

{-  TODO: I am not sure why it has to be that involved.
          It seems to me, that this function should just construct the normalizing substitution.
          That substitution should then just be applied using a standard and already implemented `apply` method.
          Is there a reason why such thing would not work?  -}
-- OLD VERSION
-- normalize :: Sigma'Type -> Sigma'Type
-- normalize (T'Forall vars q't) = T'Forall (fmap snd ord) (norm'qual'type q't)
--   where
--     pairs = zip vars letters
--     ord = map (\ (tv@(T'V tv'name kind'), fresh'name) -> (tv, T'V fresh'name kind')) pairs
--     -- NOTE: changed while h-in-h refactoring
--     -- to take advantage from T'V

--     norm'qual'type :: Qualified Type -> Qualified Type
--     norm'qual'type (preds :=> type')
--       = norm'preds preds :=> norm'type type'

--     -- TODO: napada me, ze by to asi spis melo byt tak, ze bych mel prejmenovat ty promenny,
--     -- ktery jsou v tom volny a jsou urceny k prejmenovani/normalizaci z top level kontextu
--     -- ty zavreny bych asi pak mel v klidu normalizovat pomoci normalize az po tom co to dokoncim
--     -- - normalize by pak mel splnit to, ze bude jakoukouliv volnou promennou ignorovat

--     norm'preds :: [Predicate] -> [Predicate]
--     norm'preds = map norm'pred

--     norm'pred :: Predicate -> Predicate
--     norm'pred (Is'In cl'name type') = Is'In cl'name $ norm'type type'

--     norm'type :: Type -> Type
--     norm'type (T'App a b)
--       = T'App (norm'type a) (norm'type b)
--     norm'type t@(T'Con _)
--       = t
--     norm'type (T'Tuple ts)
--       = T'Tuple $ map norm'type ts
--     norm'type (T'Var tv) =
--       case lookup tv ord of
--         Just tvar -> T'Var tvar
--         Nothing -> error $ "Type variable " ++ show tv ++ " not in the signature."
--     norm'type (T'Forall tvs (preds :=> type')) =
--       -- NOTE: local tvs which might potentially shadow the global vars must not be normalized according the global substitution
--         T'Forall tvs (norm'preds preds :=> norm'type type')
--     -- TODO: i tady je potreba zvazit, jestli tohle je OK - i kdyz tohle vypada smysluplne


generalize :: Type'Env -> Qualified Type -> Sigma'Type
generalize env qual'type
  = T'Forall vars qual'type'
    where
      fvt :: Set.Set M'V
      fvt = free'vars qual'type
      fve :: Set.Set M'V
      fve = free'vars env
      generalized = Set.toList $ fvt `Set.difference` fve
      vars = map (\ (Tau n k) -> T'V' n k) generalized
      sub = Sub $ Map.fromList $ zipWith (\ m tv -> (m, T'Var' tv)) generalized vars
      qual'type' = apply sub qual'type


{-  Utilities for working with Type Contexts  -}

{-                   fixed     gs        preds 
fixed - variables which APPEAR in the typing context (we shouldn't quantify over those, I think)
gs - set of variables we would like to quantify over
preds - predicates to split
-}

-- ten prvni list jsou volny "meta" promenny ktery jsou volny v celem typing contextu
-- ten druhej list, jsou volny "meta" promenny, ktery jsou volny v typu, ale nejsou volny v typovem contextu, takze tyhle budou generalizovany
split :: MonadError Error m => Class'Env -> [M'V] -> [M'V] -> [Predicate] -> m ([Predicate], [Predicate])
split cl'env fixed'vars gs preds = do
  preds' <- reduce cl'env preds
  let (deffered'preds, retained'preds) = partition (all (`elem` fixed'vars) . free'vars) preds'
  retained'preds' <- defaulted'preds cl'env (fixed'vars ++ gs) retained'preds
  return (deffered'preds, retained'preds \\ retained'preds')

{-  NOTE: This version of split is for Explicit and Method.
          The reason is - when I type check Explicit, I get some predicates, but because the type checking was done with skolem variable
          and not with a fresh meta type variable, this leads to those predicates containing skolem type variables and not meta variables
          for that reason when I call free'vars on the pred it will ignore the unquantified skolem variables.
          And because the way `all` works, if it's given an empty list, it will say all members passed the predicate check.
          For that reason I also check that there is no free skolem variables and if yes, then it can not pass.
          That should always work, because I was given a list of meta variables so any skolem is definitely not going to be able to pass
          (to be a member of a list of meta variables)
          one interesting thing occured to me now - 
          maybe I could change this split' to take one more argument - a list of skolems (I get those from call to `skolemise`)
          and it will check against that list explicitly -}
split' :: MonadError Error m => Class'Env -> [M'V] -> [T'V'] -> [M'V] -> [Predicate] -> m ([Predicate], [Predicate])
split' cl'env fixed'vars skolems gs preds = do
  preds' <- reduce cl'env preds
  let (deffered'preds, retained'preds) = partition part' preds'
      part' pred = (all (`elem` fixed'vars) . free'vars $ pred) && not (any (`elem` skolems) (free'vars pred :: Set.Set T'V'))
      -- fr'p = map free'vars preds' :: [Set.Set M'V]
  retained'preds' <- defaulted'preds cl'env (fixed'vars ++ gs) retained'preds
  return (deffered'preds, retained'preds \\ retained'preds')


with'defaults :: MonadError Error m => ([Ambiguity] -> [Type] -> a) -> Class'Env -> [M'V] -> [Predicate] -> m a
with'defaults fn cl'env vars preds = do
  let vps = ambiguities cl'env vars preds
  tss <- mapM (candidates cl'env) vps

  if any null tss
    then throwError $ Unexpected $ "cannot resolve ambiguity: " ++ show vps
    else return $ fn vps $ map head tss


defaulted'preds :: MonadError Error m => Class'Env -> [M'V] -> [Predicate] -> m [Predicate]
defaulted'preds = with'defaults (\ vps ts -> concatMap snd vps)


default'subst :: MonadError Error m => Class'Env -> [M'V] -> [Predicate] -> m (Subst M'V Type)
default'subst cl'env vars preds = do
  pairs <- with'defaults (zip . map fst) cl'env vars preds

  return $ Sub $ Map.fromList pairs


{-  Utilities for working with Rank-N Types -}

unify'fun :: Rho'Type -> Type'Check (Rho'Type, Rho'Type)
unify'fun (arg't `T'Fun` res't)
  = return (arg't, res't)

unify'fun fun't = do
  arg't <- fresh'meta
  res't <- fresh'meta
  let constraint = fun't `Unify` (arg't `T'Fun` res't)
  add'constraints [constraint]
  return (arg't, res't)


unify'pair :: Rho'Type -> Type'Check (Rho'Type, Rho'Type)
unify'pair (T'Tuple [first't, second't])
  = return (first't, second't)

unify'pair tuple't = do
  first't <- fresh'meta
  second't <- fresh'meta
  let constraint = tuple't `Unify` T'Tuple [first't, second't]
  add'constraints [constraint]
  return (first't, second't)


{-  OUTWARD INVARIANT: All meta type variables have kind `*`. -}
fresh'meta :: Type'Check Type
fresh'meta = do
  fresh'name <- fresh
  return $ T'Meta $ Tau fresh'name K'Star


-- NOTE: I feel like this function should be named `pr` and the skolemisation should be a detail of that operation
skolemise :: Sigma'Type -> Type'Check ([T'V'], [Predicate], Rho'Type)
skolemise (T'Forall tvs (preds :=> type')) = do                     --  PRPOLY
  skolems <- new'skolem'vars tvs
  let mapping = zipWith (\ tv skol -> (tv, T'Var' skol)) tvs skolems
      subst   = Sub $ Map.fromList mapping
  
  let preds'subst = apply subst preds

  (skolems', preds', rho) <- skolemise $ apply subst type'

  return (skolems ++ skolems', preds'subst ++ preds', rho)

skolemise (arg't `T'Fun` res't) = do                                --  PRFUN
  (skolems, preds, rho) <- skolemise res't
  return (skolems, preds, arg't `T'Fun` rho)

skolemise type'                                                     --  MONO
  = return ([], [], type')


new'skolem'vars :: [T'V'] -> Type'Check [T'V']
new'skolem'vars tvs = do
  let names = map (\ (T'V' name _) -> name) tvs
  
  mapM (\ (T'V' name kind) -> real'fresh names name >>= \ fresh'name -> return $ T'V' fresh'name kind) tvs

  -- mapM (\ (T'V name k) -> do
  --   { fresh'name <- real'fresh names name
  --   ; return $ T'V fresh'name k
  -- }) tvs


tc'rho :: Expression -> Expected Rho'Type -> Type'Check (Expression, [Predicate], Actual Rho'Type)
tc'rho = infer'expr


check'rho :: Expression -> Rho'Type -> Type'Check (Expression, [Predicate])
check'rho expr ty = do
  result <- tc'rho expr (Check ty)
  case result of
    (_, _, Inferred _) -> throwError $ Unexpected "Internal Error while 'check'rho'"
    (expr', preds, Checked) -> do
      return (expr', preds)


infer'rho :: Expression -> Type'Check (Expression, [Predicate], Rho'Type)
infer'rho expr = do
  result <- tc'rho expr Infer
  case result of
    (_, _, Checked) -> throwError $ Unexpected "Internal Error while 'infer'rho'"
    (expr', preds, Inferred type') -> do
      return (expr', preds, type')


inst'sigma :: Sigma'Type -> Expected Rho'Type -> Type'Check ([Predicate], Actual Type)
inst'sigma sigma Infer = do
  preds :=> type' <- instantiate sigma
  return (preds, Inferred type')
inst'sigma sigma (Check rho) = do
  preds <- subs'check'rho sigma rho
  return (preds, Checked)


-- TODO: I think this function needs to deal with contexts too.
-- We don't know whether they are sigma or rho types
-- but I think that if they are both sigma types, preds'r and preds
-- need to be checked, the SUB-type of the other (the more general)
-- MUST BE more general with regard to the context
-- if OK -> the corresponding constraints should be discharched, I think

-- BUT, what if one of them is sigma and the other rho?
-- and what if the sigma one wants to introduce some predicates?
-- then I can't check that parts of the rho one satisfy the same context
-- then I would probably need to introduce those? like return them
-- and let the outer scope figure the context out


-- I think that the one on the left (sigma'l) needs to be more general.
-- That means that if there is some context in (sigma'l) then 
-- the context of (sigma'r) must entail it.
-- TODO: make sure that this happens!!!
-- I think I might need to check if (sigma'r) is actually a forall.
-- If it is, I think I will need to do almost the same thing, except
-- that this function (subs'check) now becomes "the boundary" aka
-- the keeper of the (potentially) stronger context
-- and when the (subs'check'rho) returns predicates


-- tau and tau
-- if both are simple tau types (mono types) - no forall anywhere
-- then there're no contexts to deal with

-- forall and forall
-- if the left is a forall and the right is a forall
-- the context of the right one needs to entail the left one
-- question: Do I return the stronger, right context? Or do I just discharge it? Would that be even legal?
-- what if I return both?
-- the duplicated Predicates should be reduced anyway, there might be some more stuff going on
-- I would return that more stuff anyway
-- and those duplicites that I think should be discharged - will they be discharged anyway?
-- Well those skolems will have to unify with something right?
-- Maybe not, because this operation does not produce a type, just predicates.
-- WOW - this is true. If we don't produce type, just predicates, then what would be the point
-- of returning predicates that can only mention skolems/rigid variables that never exist anywhere
-- in the typing assumptions?
-- That won't be the case in general right? Because I can have scoped variables,
-- so this means that type annotations (even inside a forall) might contain those "free"
-- type variables that are bound within an outer scope
-- so some of those predicates might contain such a variable
-- 
-- so am I saying that returning those predicates will only do something if they contain
-- variables reachable from the typing assumptions and typing constranits?
-- I think I am saying that.
-- So me returning any predicates at all (now) when both are foralls
-- has no effect, those are probably not solved right?
-- Well that is not true!
-- Contexts are reduced! And when it would not be possible to reduce it fully
-- or default it, there would be an error.
-- So I think it's rather that I wasn't able to write a program
-- that would exhibit that - that would lead to this specific subs-check
-- with two foralls where one would be weaker (the wrong one)
-- but at the same time those types would only be skolemised here -
-- that would probably mean that the usual RHO type from CHECK mode
-- will not be it
-- but maybe some type where one of them is from type annotation (a sigme)
-- and the other one is a rho type, a function type
-- and it's higher-rank type (higher-rank type in the annotation)
-- so the subs'check'fun might lead to this
-- this function flips the position of arguments, but that should be fine if both are foralls anyway

-- forall and rho
-- I think the only way to ensure that the context of the right one
-- is stronger as/than the context of the left one
-- is to propagate the context (after the unification happens) to the level above
-- so that it can bubble up and eventually stop at the generalization boundary for the right type
-- that's the point where type variables from the right type are generalized
-- and that is the point where either the expression is not annotated
-- then the context is just discharged into the type of the expression (not just identifier, it can be an inline annotation)
-- or there is annotation and at that point it will be checked that the annotation has as strong/stronger context as the one
-- that the analysis figured out
-- it is similar as with the situation where I would have
-- foo :: a -> a
-- foo x = x + x
-- this would be problematic
-- but this situation is bit different, the context would not come from an expression but rather
-- from a type-level expressions
-- maybe something like this:
-- foo :: a -> a
-- foo x = x :: forall o . Num o => o
-- I think this would be the case I am talking about
-- since x is supposed to be something, and I am adding another restriction
-- saying that the type of x is actually (Num a) and that is not part of the original
-- context, I think this should fail
-- in Haskell it needs to be writen like this:
-- should'fail :: forall a . a -> a
-- should'fail x = x :: Num a => a
-- and scoped type variables need to be enabled, but I think this might be what I am thinking about

-- rho and forall
-- because rho can't have a context (at this specific level)
-- it can't introduce it (the context)
-- so whatever context the right sigma has, it is definitelly stronger
-- at this level
-- on the other hand -- there might be some predicates bound to variables in the left rho
-- so I can't just say that the right thing is stronger
-- becase it might not be?
-- so I guess this should mean that after the unification (which, again, happens for sure)
-- the context from the right sigma should be returned?
-- this seems to make sense in theory
-- I need to come up with some example of it
--
--
subs'check :: Sigma'Type -> Sigma'Type -> Type'Check [Predicate]
subs'check sigma'l sigma'r = do
  (skolems, preds'r, rho'r) <- skolemise sigma'r
  preds <- subs'check'rho sigma'l rho'r

  constraints <- get'constraints
  case run'solve constraints :: Either Error (Subst M'V Type) of
    Left err -> throwError err
    Right subst -> do
      let sigma'l' = apply subst sigma'l
          sigma'r' = apply subst sigma'r
      
      let all'free = free'vars [sigma'l', sigma'r']

      let bad'vars = filter (`elem` all'free) skolems

      if not $ null bad'vars
      then throwError $ Unexpected "Subsumption check failed! - Probable reason: Impredicative Polymorphism."
      else return (preds'r ++ preds)


-- Invariant: the second argument is in weak-prenex form
-- TODO: I think that this function should be checking that the contexts are valid.
-- It checks that one type is a subtype of the other one.
-- To be more specific, that the first argument (sigma type) is a sub-type of the second one (rho type).
-- In other words, the first one must be MORE general/polymorphic than the second one.
-- But that means, that whatever context sigma keeps, rho must obey it too.
subs'check'rho :: Sigma'Type -> Rho'Type -> Type'Check [Predicate]
subs'check'rho sigma@(T'Forall _ _) rho = do                              --  SPEC
  preds :=> rho' <- instantiate sigma
  -- So since the sigma is supposed to be the MORE GENERAL one
  -- that implies that rho can never lose the context of sigma
  -- rho can be stricter - meaning - it can add more stuff into its context
  -- making it stricter/less general, but it can never NOT have something
  -- that sigma has
  -- so (preds) need to be returned for sure
  
  -- here (preds) are just from the sigma one 
  preds' <- subs'check'rho rho' rho
  -- here (preds') can be from both, because subs'check
  -- introduces Predicates from both types into the system
  return (preds ++ preds')

subs'check'rho rho'l (arg'r `T'Fun` res'r) = do                           --  FUN1
  (arg'l, res'l) <- unify'fun rho'l
  subs'check'fun arg'l res'l arg'r res'r

subs'check'rho (arg'l `T'Fun` res'l) rho'r = do                           --  FUN2
  (arg'r, res'r) <- unify'fun rho'r
  subs'check'fun arg'l res'l arg'r res'r

subs'check'rho tau'l tau'r = do                                           --  MONO
  add'constraints [tau'l `Unify` tau'r]
  return []


subs'check'fun :: Sigma'Type -> Rho'Type -> Sigma'Type -> Rho'Type -> Type'Check [Predicate]
subs'check'fun a1 r1 a2 r2 = do
  preds <- subs'check a2 a1
  preds' <- subs'check'rho r1 r2

  return (preds ++ preds')

-- Tahle funkce bude orisek
-- kdyz skolemisuju sigmu, dostanu qualified type
-- a zda se me, ze pokud bych do check'rho poslal jenom rho
-- mohl bych ztratit tu informaci, ze expr "domnele" pozaduje nejaky Predicates
-- hodne zalezi na tom, jak vlastne funguje check'rho
-- pokud je tam nekde unifikace na bottom urovni, tak by snad mohlo stacit vratit ty Predicates
-- protoze by se melo stat to, ze skolemy (ty jediny jsou kvalifikovany kontextem) se zunifikuji
-- s odpovidajicimy meta promennymi (s nicim jinym ani nemuzou, krome sebe a to tady moc nepomuze)
-- 
-- na druhou stranu, je opravdu potreba to poradne promyslet, je klidne mozny, ze funkce, ktery operujici jako check'rho
-- tedy nad rozbalenou sigmou, budou ve skutecnosti muset brat qualified rho
-- otazka by pak ale byla - a to dost zasadni
-- co se takhle uplne dole bude s Predicates delat?
-- co s nima bude delat check'rho
--
-- dalsi moznost co me napada, je ze by se mohlo dat udelat to, co se dela na top levelu
-- vyresit constrainty a sestavit substituci, aplikovat ji a pak zkusit zredukovat kontext a uvidet
-- jestli je context OK, nebo je moc weak, nebo tak neco
-- obavam se ale - ze to uplne nepujde v pripadech kdy nepujde o uplne primitivni expressions a typy
-- napriklad 23 :: Int -- tohle je v pohode, (ale nevim, jestli tenhle pripad skonci tady, to je dost mozny ze vubec ne)
-- napriklad (\ a -> show a) :: forall a . Show a => a -> String
-- tak tohle se snad da zkontrolovat
-- 
{-  TODO: This function will need to be refactored, fixed and checked thoroughly. -}
check'sigma :: Expression -> Sigma'Type -> Type'Check (Expression, [Predicate])
check'sigma expr sigma = do
  (skolems, preds, rho) <- skolemise sigma
  (expr', preds') <- check'rho expr rho
  -- now I need to solve constraints, obtain the substitution, apply it to the sigma and the typing context
  -- then I need to check that sigma, nor types in typing context contain any of the skolems
  --
  -- k tomuhle mam zajimavou poznamku, jak to mam momentalne, tak nemam constrainty ze zbyvajiciho systemu
  -- mam jenom ty lokalni
  -- a zajimalo by me, jestli lokalni constrainty staci prozkoumat
  -- pokud staci, tak me zajima, jestli se pak musim starat o typy v typing contextu
  -- nebo jestli staci zkontrolovat, ze se zadny skolem nedostane do sigmy
  --
  -- az to upravim tak, aby se constrainty ukladaly do state
  -- tak budu mit pristup ke vsem ode vsad
  -- pak tahle poznamka nebude nejspis vubec relevantni
  constraints <- get'constraints
  case run'solve constraints :: Either Error (Subst M'V Type) of
    Left err -> throwError err
    Right subst -> do
      type'ctxt <- asks type'env
      let type'ctxt'  = apply subst type'ctxt
          sigma'      = apply subst sigma

      let free'ctxt   = free'vars type'ctxt'
          free'sigma  = free'vars sigma'
          free'all    = free'ctxt `Set.union` free'sigma

      let bad'vars  = filter (`elem` free'all) skolems


      -- NOTE: This is pretty much an experiment to get rid of the bug where the type system allows type annotations and application of higher-rank function type
      -- to cause loosening the type of an expression.
      -- EXAMPLES:
      -- TODO: ADD THOSE

      -- maybe I should deal with the outer context only if the sigma is really a sigma
      -- because if it is just some rho type then it does not really make sense to do all this
      -- because:
      -- skolemise for a rho type returns an empty list of predicates - which makes total sense
      -- but that empty set of predicates can not entail possible set of predicates raised from the expression
      -- EXAMPLE:
      -- ; an'example :: Show a => a -> [Char]
      -- ; an'example a = show a ++ "."
      -- where the issue is with (++)'s type ((++) :: [a] -> [a] -> [a]) ;
      -- namely the part where the argument type (sigma or rather rho, really)
      -- being [a]
      -- can not really produce a context that would entail the context of the expression (show a)
      -- which is (Show a') where (a') is the type of (a)
      -- so I am moving this whole expriment behind a check that (sigma) really is a Forall-ed type
      case sigma of
        T'Forall _ _ -> do
          Infer'Env{ class'env = c'env } <- ask
          let outer'context = apply subst preds   -- this context must not be weaker than the one that has been produced by the actual checking the expression
              inner'context = apply subst preds'  -- this is the inner context from the expression
              
          outer'reduced <- reduce c'env outer'context
          inner'reduced <- reduce c'env inner'context

          -- the outer'reduced must entail the whole inner'reduce (or so I believe)

          let ps' = filter (not . entail c'env outer'reduced) inner'reduced


          if not $ null ps'
          then do
            let message = ("----------\ncheck'sigma\nsigma = " ++ show sigma ++ "\n") ++
                          ("skolems = " ++ show skolems ++ "\npreds = " ++ show preds ++ "\nrho = " ++ show rho) ++ "\n" ++
                          ("preds' = " ++ show preds' ++ "\n") ++
                          ("check'sigma\n" ++ "outer'context = " ++ show outer'context ++ "\ninner'context = " ++ show inner'context ++ "\n") ++
                          ("check'sigma\n" ++ "outer'reduced = " ++ show outer'reduced ++ "\ninner'reduced = " ++ show inner'reduced ++ "\n") ++
                          ("the expression = " ++ show expr ++ "\n\nsubst = " ++ show subst)
            throwError $ Unexpected ("The context is too weak!\n" ++ "Predicates: " ++ show ps' ++ "\n can not be solved." ++ "\n" ++ message ++ "\n\n")
          else return (expr', outer'reduced)
          -- IMPORTANT: I think that if the context from the type entails the whole inferred context
          -- it should be OK to return only the outer'reduced context.
          -- The reason being, the inner is already contained within the outer one and the outer one
          -- may contain some additional restrictions so we want to propagate those.
          -- I need to test it and think it through though!

          -- so is it OK to just return the outer (the one that should entail the inner?)
          -- since it entails the inner I think it should be OK, I guess
          -- it can contain some more predicates than just the ones in the innner

        _ -> do
          if not $ null bad'vars
          then
            throwError $ Unexpected "Type is not polymorphic enough!"
          else do
            return (expr', preds ++ preds')
            -- I still return those predicates for now
            -- in the future, I think it would be worth thinking about whether some of them can actually be eliminated completely

      -- end of the experiment
      --


      -- if not $ null bad'vars
      -- then
      --   throwError $ Unexpected "Type is not polymorphic enough!"
      -- else do
      --   return (expr', preds ++ preds')
      --   -- I still return those predicates for now
      --   -- in the future, I think it would be worth thinking about whether some of them can actually be eliminated completely


-- TODO:  tady budu potrebovat trosku vymyslet neco vlastniho
--        jak jsem se uz rozhodl, bude to spocivat v tom, ze posbiram constrainty, vyresim je, aplikuju substituci
--        na sigmu, do ktere by tim padem mohly "leaknout" nektere skolemy, to zkontroluju zavolanim free'vars
--        a checknutim, ze nema zadny prunik se skolem setem



split'data'cons :: Rho'Type -> ([Sigma'Type], Tau'Type)
split'data'cons (arg'type `T'Fun` res'type) = (arg'type : arg'types, out'type)
  where (arg'types, out'type) = split'data'cons res'type
split'data'cons rho = ([], rho)






-- stuff for class desugaring

lookup'in'overloaded :: Name -> Type'Check (Maybe Overloaded)
lookup'in'overloaded name = do
  overloaded' <- asks overloaded
  return $ lookup name overloaded'
  -- asks (lookup name . overloaded)


-- this function needs to eliminate the placeholders in the Match
phs'matches :: Subst M'V Type -> Match -> Match
phs'matches _ x = x