{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE FlexibleContexts #-}

module Compiler.TypeSystem.Utils.Infer where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List ( partition, (\\), zipWith )
import Data.Bifunctor ( Bifunctor(second) )

import Control.Monad.Reader ( asks, MonadReader(local) )
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
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(Infer'Env, type'env, kind'env, constraint'env, overloaded, instance'env, instances), Type'Env )
import Compiler.TypeSystem.ClassEnv ( Class'Env )
import Compiler.TypeSystem.Ambiguity ( ambiguities, candidates, Ambiguity )
import Compiler.TypeSystem.Solver.Solve ( Solve )
import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Utils.Class ( reduce )
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


lookup'dict :: (Name, Type) -> Type'Check Name
lookup'dict placeholder = do
  env <- asks instance'env
  case lookup placeholder env of
    Nothing -> throwError $ Unexpected ("Can't find dictionary for " ++ show placeholder)
    Just name -> return name


lookup'instance :: (Name, Type) -> Type'Check (Name, [Predicate], Predicate)
lookup'instance placeholder = do
  env <- asks instances
  case lookup placeholder env of
    Nothing -> throwError $ Unexpected ("Can't find an instance for " ++ show placeholder)
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
split :: Class'Env -> [M'V] -> [M'V] -> [Predicate] -> Solve ([Predicate], [Predicate])
split cl'env fixed'vars gs preds = do
  preds' <- reduce cl'env preds
  let (deffered'preds, retained'preds) = partition (all (`elem` fixed'vars) . free'vars) preds'
  retained'preds' <- defaulted'preds cl'env (fixed'vars ++ gs) retained'preds
  return (deffered'preds, retained'preds \\ retained'preds')

{-  NOTE: This version of split is for Explicit and Method.
          The reason is - when I type check Explicit, I get some predicates, but because the type checking was done with skolem variable
          and not with a fresh meta type variable, this leads to those predicates containing skolem type variables and not meta variables
          for that reason when I cann free'vars on the pred it will ignore the unquantified skolem variables.
          And because the way `all` works, if it's given an empty list, it will say all members passed the predicate check.
          For that reason I also check that there is no free skolem variables and if yes, then it can not pass.
          That should always work, because I was given a list of meta variables so any skolem is definitely not going to be able to pass
          (to be a member of a list of meta variables)
          one interesting thing occured to me now - 
          maybe I could change this split' to take one more argument - a list of skolems (I get those from call to `skolemise`)
          and it will check against that list explicitly -}
split' :: Class'Env -> [M'V] -> [T'V'] -> [M'V] -> [Predicate] -> Solve ([Predicate], [Predicate])
split' cl'env fixed'vars skolems gs preds = do
  preds' <- reduce cl'env preds
  let (deffered'preds, retained'preds) = partition part' preds'
      part' pred = (all (`elem` fixed'vars) . free'vars $ pred) && null (free'vars pred :: Set.Set T'V')
      fr'p = map free'vars preds' :: [Set.Set M'V]
  retained'preds' <- defaulted'preds cl'env (fixed'vars ++ gs) retained'preds
  return (deffered'preds, retained'preds \\ retained'preds')


with'defaults :: ([Ambiguity] -> [Type] -> a) -> Class'Env -> [M'V] -> [Predicate] -> Solve a
with'defaults fn cl'env vars preds = do
  let vps = ambiguities cl'env vars preds
  tss <- mapM (candidates cl'env) vps

  if any null tss
    then throwError $ Unexpected $ "cannot resolve ambiguity: " ++ show vps
    else return $ fn vps $ map head tss


defaulted'preds :: Class'Env -> [M'V] -> [Predicate] -> Solve [Predicate]
defaulted'preds = with'defaults (\ vps ts -> concatMap snd vps)


default'subst :: Class'Env -> [M'V] -> [Predicate] -> Solve (Subst M'V Type)
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
subs'check'rho :: Sigma'Type -> Rho'Type -> Type'Check [Predicate]
subs'check'rho sigma@(T'Forall _ _) rho = do                              --  SPEC
  preds :=> rho' <- instantiate sigma
  preds' <- subs'check'rho rho' rho
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
-- protoze by se melo stat to, ze skolemy (ty jediny jsou kvalifikovany kontextem) se sunifikuji
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

      if not $ null bad'vars
      then
        throwError $ Unexpected "Type is not polymorphic enough!" -- \n| sigma: " ++ show sigma ++ " \n| sigma': " ++ show sigma' ++ "  \n| skolems: " ++ show skolems ++ " \n| free'all: " ++ show free'all ++ "\n| free'sigma: " ++ show free'sigma ++ "\n| skolemized: " ++ show rho
      else do
        return (expr', preds ++ preds')


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