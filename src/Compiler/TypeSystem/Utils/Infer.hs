{-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.TypeSystem.Utils.Infer where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List ( partition, (\\) )
import Data.Bifunctor ( Bifunctor(second) )

import Control.Monad.Reader ( asks, MonadReader(local) )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Counter ( Counter(Counter, counter), fresh, real'fresh, letters )

import Compiler.Syntax.HasKind ( HasKind(kind) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified(..) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V(..), Type(T'Forall, T'Var) )

import Compiler.TypeSystem.Error ( Error(Unexpected, Unbound'Var, Unbound'Type'Var) )
import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.InferenceEnv ( Class'Env, Infer'Env(Infer'Env, type'env, kind'env), Type'Env )
import Compiler.TypeSystem.Ambiguity ( ambiguities, candidates, Ambiguity )
import Compiler.TypeSystem.Solver.Solve ( Solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Utils.Class ( reduce )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term(free'vars) )


-- NOTE: temporarily I will put it here
quantify :: [T'V] -> Qualified Type -> Sigma'Type
quantify = T'Forall
-- now, here's the thing
-- because I keep both names and kinds of the parametrized type variables
-- as opposed to only keeping kinds (as does Jones)
-- I don't really need to do anything about vars

-- similarly, because I don't represent bound type variables with distinct data construct (TGen)
-- I don't really need to do any substitution application on the qualified type

-- now, that begs the question, do I really need to have this function?
-- I think not.


to'scheme :: Type -> Sigma'Type
to'scheme sigma@(T'Forall _ _) = sigma
to'scheme t = T'Forall [] ([] :=> t)

{- Qualifies the Type with no Predicates -}
qualify :: Type -> Qualified Type
qualify t = [] :=> t


-- TODO: I really feel like generalizing all the merge'into'... and put'in'... and lookup'...
--        is the best way to go around. Then put them in some shared Utils module and use them
--        across all parts of the pipeline.
merge'into't'env :: [(String, Sigma'Type)] -> Infer a -> Infer a
merge'into't'env bindings m = do
  let scope e@Infer'Env{ type'env = t'env } = e{ type'env = Map.fromList bindings `Map.union` t'env}
  local scope m


put'in't'env :: (String, Sigma'Type) -> Infer a -> Infer a
put'in't'env (var, scheme) m = do
  let scope e@Infer'Env{ type'env = t'env } = e{ type'env = Map.insert var scheme (Map.delete var t'env) }
  local scope m


{- TODO NOTE: What about creating some type class for Kind, Type and such
- it would define lookup method and the correct return type would be figured from the type "context"
(as in - call site) -}
lookup't'env :: String -> Infer (Qualified Type)
lookup't'env var = do
  env <- asks type'env
  case Map.lookup var env of
    Nothing     ->  throwError $ Unbound'Var var
    Just scheme ->  instantiate scheme


merge'into'k'env :: [(String, Kind)] -> Infer a -> Infer a
merge'into'k'env bindings m = do
  let scope e@Infer'Env{ kind'env = k'env } = e{ kind'env = Map.fromList bindings `Map.union` k'env }
  local scope m


put'in'k'env :: (String, Kind) -> Infer a -> Infer a
put'in'k'env (var, kind') m = do
  let scope e@Infer'Env{ kind'env = k'env } = e{ kind'env = Map.insert var kind' (Map.delete var k'env) }
  local scope m


lookup'k'env :: String -> Infer Kind
lookup'k'env var = do
  k'env <- asks kind'env
  case Map.lookup var k'env of
    Nothing     -> throwError $ Unbound'Type'Var var
    Just kind'  -> return kind'


{-  TODO: Here is a BIG TODO - I need to go over the paper THIH and see if I can replace all my
          uses of instantiate and close'over with what Jones does.
          Reason: it seems that Jones always closes and generalizes only over explicitly specified variables.
          That might seem like extra work, but there's moment in the inference where it really comes in handy.
          I think it's related to the inference of explicitly annotated expressions. Not sure.
          So I wonder, if for the sake of being consistent I should replace my somehow autonomous functions with those from THIH.
          I really think it is worth considering.
          Probably also includes `normalize` and `generalize`? I am not sure what they do at this moment. Inspect yourself later.
-}
instantiate :: Sigma'Type -> Infer (Qualified Type)
instantiate (T'Forall vars qual'type) = do
  let params = map (\ (T'V name _) -> name) vars
  fresh'strs <- mapM (\ (T'V n k) -> real'fresh params n >>= \ fresh -> return (fresh, k) ) vars
  let ty'vars = map (\ (name, k) -> T'Var (T'V name k)) fresh'strs
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
  let subst = Sub $ Map.fromList $ zip vars ty'vars
  return $ apply subst qual'type

{-  NOTE: Making the function covering/total. Instantiating non forall type is just an identity.  -}
instantiate type' = return $ qualify type'


{-  TODO: Consider removing this function from the codebase.
          I might be able to use `quantify` instead.
          I would need to explicitly specify what variables I want to close over.
          But it might be a better practice.
          As being explicit might give much more information about what is the intent and what variables should be closed over.
-}
close'over :: Qualified Type -> Sigma'Type
close'over = normalize . generalize Map.empty


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
    pairs = zip vars letters
    mapping = map (\ (tv@(T'V tv'name kind'), fresh'name) -> (tv, T'V fresh'name kind')) pairs
    subst = Sub $ Map.fromList $ map (second T'Var) mapping
    vars'norm = map snd mapping
    q't'norm = apply subst q't

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
  = T'Forall vars qual'type
    where
      fvt = free'vars qual'type
      fve = free'vars env
      vars = Set.toList $ fvt `Set.difference` fve




{-  Utilities for working with Type Contexts  -}

{-                   fixed     gs        preds 
fixed - variables which APPEAR in the typing context (we shouldn't quantify over those, I think)
gs - set of variables we would like to quantify over
preds - predicates to split
-}
split :: Class'Env -> [T'V] -> [T'V] -> [Predicate] -> Solve ([Predicate], [Predicate])
split cl'env fixed'vars gs preds = do
  preds' <- reduce cl'env preds
  let (deffered'preds, retained'preds) = partition (all (`elem` fixed'vars) . free'vars) preds'
  retained'preds' <- defaulted'preds cl'env (fixed'vars ++ gs) retained'preds
  return (deffered'preds, retained'preds \\ retained'preds')


with'defaults :: ([Ambiguity] -> [Type] -> a) -> Class'Env -> [T'V] -> [Predicate] -> Solve a
with'defaults fn cl'env vars preds = do
  let vps = ambiguities cl'env vars preds
  tss <- mapM (candidates cl'env) vps

  if any null tss
    then throwError $ Unexpected "cannot resolve ambiguity"
    else return $ fn vps $ map head tss


defaulted'preds :: Class'Env -> [T'V] -> [Predicate] -> Solve [Predicate]
defaulted'preds = with'defaults (\ vps ts -> concatMap snd vps)


default'subst :: Class'Env -> [T'V] -> [Predicate] -> Solve (Subst T'V Type)
default'subst cl'env vars preds = do
  pairs <- with'defaults (zip . map fst) cl'env vars preds
  return $ Sub $ Map.fromList pairs




{-  Utilities for generating Unification Constraints  -}

infix 4 `unify'types`
unify'types :: Type -> Type -> (Constraint Type, Constraint Kind)
a `unify'types` b = (a `Unify` b, kind a `Unify` kind b)
