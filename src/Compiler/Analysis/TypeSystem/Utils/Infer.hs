module Compiler.Analysis.TypeSystem.Utils.Infer where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List (partition, (\\))


import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except


import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.InferenceState
import Compiler.Analysis.TypeSystem.Ambiguity

import Compiler.Analysis.TypeSystem.Solver.Solve
import Compiler.Analysis.TypeSystem.Solver.Substitutable

import Compiler.Analysis.TypeSystem.Utils.Class


-- NOTE: temporarily I will put it here
quantify :: [T'V] -> Qualified Type -> Scheme
quantify = For'All
-- now, here's the thing
-- because I keep both names and kinds of the parametrized type variables
-- as opposed to only keeping kinds (as does Jones)
-- I don't really need to do anything about vars

-- similarly, because I don't represent bound type variables with distinct data construct (TGen)
-- I don't really need to do any substitution application on the qualified type

-- now, that begs the question, do I really need to have this function?
-- I think not.


to'scheme :: Type -> Scheme
to'scheme t = For'All [] ([] :=> t)

{- Qualifies the Type with no Predicates -}
qualify :: Type -> Qualified Type
qualify t = [] :=> t




letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']


fresh :: Infer String
fresh = do
  Infer'State { count = counter } <- get
  put $ Infer'State { count = counter + 1 }
  return (letters !! counter)


real'fresh :: [String] -> a -> Infer String
real'fresh vars var = do
  Infer'State { count = counter } <- get
  put $ Infer'State { count = counter + 1 }
  let name = letters !! counter
  if name `elem` vars
    then real'fresh vars var
    else return name


extend :: (Ord a) => Map.Map a b -> (a, b) -> Map.Map a b
extend env (ty'var, scheme) = Map.insert ty'var scheme env


remove :: (Ord a) => Map.Map a b -> a -> Map.Map a b
remove env var = Map.delete var env


merge'into't'env :: [(String, Scheme)] -> Infer a -> Infer a
merge'into't'env bindings m = do
  let scope e@Infer'Env{ type'env = t'env } = e{ type'env = Map.fromList bindings `Map.union` t'env}
    -- (k'env, Map.fromList bindings `Map.union` t'env, ali'env)
  local scope m


put'in't'env :: (String, Scheme) -> Infer a -> Infer a
put'in't'env (var, scheme) m = do
  -- (k'env, _) <- ask
  let scope e@Infer'Env{ type'env = t'env } = e{ type'env = remove t'env var `extend` (var, scheme) }
    -- (k'env, remove t'env var `extend` (var, scheme), ali'env)
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
    -- (Map.fromList bindings `Map.union` k'env, t'env, ali'env)
  local scope m


put'in'k'env :: (String, Kind) -> Infer a -> Infer a
put'in'k'env (var, kind') m = do
  let scope e@Infer'Env{ kind'env = k'env } = e{ kind'env = remove k'env var `extend` (var, kind') }
    -- (remove k'env var `extend` (var, kind'), t'env, ali'env)
  local scope m


lookup'k'env :: String -> Infer Kind
lookup'k'env var = do
  k'env <- asks kind'env
  case Map.lookup var k'env of
    Nothing     -> throwError $ Unbound'Type'Var var
    Just kind'  -> return kind' -- we don't instantiate kinds
    -- TODO: do some normalization or something
    -- change all the free variables in the kind into *


-- put'in'ali'env :: (String, Type) -> Infer a -> Infer a
-- put'in'ali'env (name, type') m = do
--   let scope e@Infer'Env{ ali'env = a'env } = e{ ali'env = remove a'env name `extend` (name, type') }
--   local scope m


instantiate :: Scheme -> Infer (Qualified Type)
instantiate (For'All vars qual'type) = do
  let params = map (\ (T'V name _) -> name) vars
  fresh'strs <- mapM (real'fresh params) vars
  let ty'vars = map (\ name -> T'Var (T'V name K'Star)) fresh'strs -- TODO: the Star kind is incorrect
  -- it needs to be fixed promptly
  -- instead -> `vars` will (have to) contain information about which parametrized (quantified) variables have which kinds
  --
  let subst = Sub $ Map.fromList $ zip vars ty'vars
  return $ apply subst qual'type


close'over :: Qualified Type -> Scheme
close'over = normalize . generalize Map.empty


{- TODO:  Consider changing the type signature to
          Scheme -> Infer Scheme
          because norm'qual'type -}
normalize :: Scheme -> Scheme
normalize (For'All vars q't) = For'All (fmap snd ord) (norm'qual'type q't)
  where
    pairs = zip (Set.toList . free'vars $ q't) letters
    ord = map (\ (tv@(T'V tv'name kind'), fresh'name) -> (tv, T'V fresh'name kind')) pairs
    -- NOTE: changed while h-in-h refactoring
    -- to take advantage from T'V

    norm'qual'type :: Qualified Type -> Qualified Type
    norm'qual'type (preds :=> ta@(T'App _ _)) = preds :=> norm'type ta
    -- norm'type (TyArr a b) = TyArr (norm'type a) (norm'type b)
    norm'qual'type qt@(_ :=> (T'Con _)) = qt
    norm'qual'type (preds :=> tu@(T'Tuple ts)) = preds :=> norm'type tu
    norm'qual'type (preds :=> tv@(T'Var _)) = preds :=> norm'type tv
    norm'qual'type _ = error "impossible norm'qual'type"

    norm'type :: Type -> Type
    norm'type (T'App a b) = T'App (norm'type a) (norm'type b)
    norm'type t@(T'Con _) = t
    norm'type (T'Tuple ts) = T'Tuple $ map norm'type ts
    norm'type (T'Var tv) =
      case lookup tv ord of
        Just tvar -> T'Var tvar
        Nothing -> error $ "Type variable " ++ show tv ++ " not in the signature."
    norm'type _ = error "impossible norm'type"


generalize :: Type'Env -> Qualified Type -> Scheme
generalize env qual'type
  = For'All vars qual'type
    where
      fvt = free'vars qual'type
      fve = free'vars env
      vars = Set.toList $ fvt `Set.difference` fve


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
