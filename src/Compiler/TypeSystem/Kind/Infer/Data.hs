module Compiler.TypeSystem.Kind.Infer.Data where




import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Declaration ( Constr'Decl(..), Data(..) )
import Compiler.Syntax.Kind ( Kind(..) )
import Compiler.Syntax.Type ( T'C(T'C), Type(..), T'V (T'V) )

import Compiler.TypeSystem.Infer ( Infer )
import Compiler.TypeSystem.Constraint ( Constraint (Unify) )
import Control.Monad.Extra (concatMapM)
import Compiler.TypeSystem.Kind.Infer.Type (infer'type)

import Compiler.TypeSystem.Utils.Infer ( merge'into'k'env, lookup'k'env )


infer'data :: Data -> Infer ([(Name, Kind)], [(Name, Kind)], [Constraint Kind])
infer'data Data{ type'name = t'n@(T'C t'name k), type'params = t'params, constructors = cons } = do
  -- potrebuju tohle - musim vyrobit tu typovou aplikaci t'name na vsechny type parametry a tomu priradit Kind *
  -- nejsem si tim uplne jistej, ale myslim, ze to by melo uplne stacit
  -- tim chci rict, ze nebude potreba prochazet jednotlivy typovy parametry - spojit jejich Kindy do Kind'Arrow
  -- a rict, ze ten typovej constructor t'name ma kind -- ta Kind'Arrow
  -- na druhou stranu bych to asi udelat mohl
  -- ale zajimalo by me, jestli by fakt nestacilo rict
  -- ze t'name aplikovanej na t'params == *
  -- mozna bych tim mohl zacit, zkusit udelat par testu a zjistit, jestli je mozny, aby se to v nejakem edge casu pokazilo, nebo jestli je ten algoritmus sound uz tak jak to je

  -- pak jeste musim projit vsechny constructory, bude potreba projit kazdej z nich
  -- a priradit kazdemu typu jednotlivych argumentu toho construktoru *

  -- let t'app = foldl T'App (T'Con t'name) $ map T'Var t'params
      -- t'k = kind t'app `Unify` K'Star
  -- ja nemuzu tohle udelat - protoze `kind` funkce predpoklada, ze Kind'App bude strukturovana tak, ze jeji Left bude Kind'Arrow - jenze v tenhle moment to spis budou
  -- Kind'Variable vetsinou - no a to zpusobi fail
  -- napada me reseni tak, ze `kind` bude vracet krome Kindu jeste list constraintu pro to, aby to tak bylo spravne
  -- coz by vlastne udelal presne to co jsem popisoval nahore
  -- ja bych rekl - kind cele tenhla aplikace je *
  -- a uz jenom tim, ze bych rekl, ze chci `kind` te aplikace, tak by mi to vyhazelo hromadu constraintu, ktery by assertovaly, ze ta vec je validne strukturovana 
  -- to mi prijde jako zajimavy napad - musim zkontrolovat kde vsude tuhle funkci `kind` pouzivam

  -- prozatim to obejdu a vyresim to tady na miste
  -- TODO: ale zvaz upraveni `kind` funkce - v tom pripade je fakt se potreba ujistit, ze to bude spravne fungovat vsude kde se to bude pozuivat
  -- let k'arr = foldr (\ (T'V _ k) k'acc -> K'Arr k k'acc ) K'Star t'params
  --     k'constr = k `Unify` k'arr
  --     constructors'k'cs = concatMap collect'con cons
  -- in  k'constr : constructors'k'cs

  -- to nahore taky neni spravne
  -- od ceho mam kind inferenci?


  --  Get the Kind of the Type Constructor itself
  kind <- lookup'k'env t'name

  let assumptions = map (\ (T'V name kind) -> (name, kind)) t'params

  let t = foldl (\ t'acc t'v -> T'App t'acc $ T'Var t'v) (T'Con t'n) t'params -- Type Name applied to all type parameters
  (k, k'cs) <- merge'into'k'env assumptions (infer'type t) -- merging assumptions about data type variables and their kinds
  let k'c = k `Unify` K'Star
  constructors'k'cs <- merge'into'k'env assumptions (concatMapM collect'con cons) -- TODO: Will it work like this? Amazing!
  let constraints = k'c : k'cs ++ constructors'k'cs

  let type'assumptions = [(t'name, kind)] -- assigning Type defined by this `data` its inferred Kind

  return (type'assumptions, [], constraints)
  
  
infer'n'unify :: Type -> Infer [Constraint Kind]
infer'n'unify t = do
  (k, k'cs) <- infer'type t
  return $ (k `Unify` K'Star) : k'cs


collect'con :: Constr'Decl -> Infer [Constraint Kind]
collect'con (Con'Decl _ types) = do
  -- kinds <- foldM infer'many [] types
  concatMapM infer'n'unify types

collect'con (Con'Record'Decl _ fields) = do
  concatMapM infer'n'unify $ map snd fields



-- {-  This is a previou simplementation of Kind Inference - it was much more intertwined with the Type Inference  -}
-- infer'kinds :: Program -> [Constraint Kind] -> Infer (Kind'Env, Subst Name Kind)
-- infer'kinds Program{ method'annotations = m'anns } k'cs = do
--   k'data'constrs <- concatMapM collect data'decls
--   k'method'constrs <- concatMapM infer'method m'anns

--   -- TODO:  I think I will need to collect all type constructors declared by the user and associate them with their Kinds.
--   --        That will give me a Kind Environment which I can later apply the Kind Substitution to.
--   --        Maybe I do already have that in the Program?

--   case run'solve (k'cs ++ k'data'constrs ++ k'method'constrs) :: Either Error (Subst Name Kind) of
--     Left err -> throwError err
--     Right subst -> do
--       Infer'Env{ kind'env = k'env } <- ask
--       Counter{ counter = counter } <- get
--       return (apply subst k'env, subst)


--     where
--       collect :: Data -> Infer [Constraint Kind]
--       collect Data{ type'name = t'n@(T'C t'name k), type'params = t'params, constructors = cons } = do
--         -- potrebuju tohle - musim vyrobit tu typovou aplikaci t'name na vsechny type parametry a tomu priradit Kind *
--         -- nejsem si tim uplne jistej, ale myslim, ze to by melo uplne stacit
--         -- tim chci rict, ze nebude potreba prochazet jednotlivy typovy parametry - spojit jejich Kindy do Kind'Arrow
--         -- a rict, ze ten typovej constructor t'name ma kind -- ta Kind'Arrow
--         -- na druhou stranu bych to asi udelat mohl
--         -- ale zajimalo by me, jestli by fakt nestacilo rict
--         -- ze t'name aplikovanej na t'params == *
--         -- mozna bych tim mohl zacit, zkusit udelat par testu a zjistit, jestli je mozny, aby se to v nejakem edge casu pokazilo, nebo jestli je ten algoritmus sound uz tak jak to je

--         -- pak jeste musim projit vsechny constructory, bude potreba projit kazdej z nich
--         -- a priradit kazdemu typu jednotlivych argumentu toho construktoru *

--         -- let t'app = foldl T'App (T'Con t'name) $ map T'Var t'params
--             -- t'k = kind t'app `Unify` K'Star
--         -- ja nemuzu tohle udelat - protoze `kind` funkce predpoklada, ze Kind'App bude strukturovana tak, ze jeji Left bude Kind'Arrow - jenze v tenhle moment to spis budou
--         -- Kind'Variable vetsinou - no a to zpusobi fail
--         -- napada me reseni tak, ze `kind` bude vracet krome Kindu jeste list constraintu pro to, aby to tak bylo spravne
--         -- coz by vlastne udelal presne to co jsem popisoval nahore
--         -- ja bych rekl - kind cele tenhla aplikace je *
--         -- a uz jenom tim, ze bych rekl, ze chci `kind` te aplikace, tak by mi to vyhazelo hromadu constraintu, ktery by assertovaly, ze ta vec je validne strukturovana 
--         -- to mi prijde jako zajimavy napad - musim zkontrolovat kde vsude tuhle funkci `kind` pouzivam

--         -- prozatim to obejdu a vyresim to tady na miste
--         -- TODO: ale zvaz upraveni `kind` funkce - v tom pripade je fakt se potreba ujistit, ze to bude spravne fungovat vsude kde se to bude pozuivat
--         -- let k'arr = foldr (\ (T'V _ k) k'acc -> K'Arr k k'acc ) K'Star t'params
--         --     k'constr = k `Unify` k'arr
--         --     constructors'k'cs = concatMap collect'con cons
--         -- in  k'constr : constructors'k'cs

--         -- to nahore taky neni spravne
--         -- od ceho mam kind inferenci?
--         let t = foldl (\ t'acc t'v -> T'App t'acc $ T'Var t'v) (T'Con t'n) t'params
        
--         (k, k'cs) <- infer'type t

--         let k'c = k `Unify` K'Star
        
--         constructors'k'cs <- concatMapM collect'con cons

--         return $ k'c : k'cs ++ constructors'k'cs

--       infer'method :: (Name, Sigma'Type) -> Infer [Constraint Kind]
--       infer'method a@(n, T'Forall tvs (context :=> type')) = do
--         -- NOTE:  I don't care about the Context
--         -- TODO:  NEEE nesmim ignorovat kontext
--         --        musim ho vzit a posbirat constrainty i z nej
--         -- all type variables there are just going to be in Predicates like (Show a), (Read a) and never like (Foo (m a))
--         --
--         ctxt'k'cs <- infer'context context

--         infer'n'unify type'
--         -- (k, k'cs) <- infer'type type'
--         -- let cs =  (k `Unify` K'Star) : k'cs

--         -- let tt = trace ("\n\n[++tracing++]   a:  " ++ show a ++ "\n  k:  " ++ show k ++ "\n  cs:  " ++ show cs) cs

--         -- return tt

--       {- THIS IS NEW -}
--       infer'context :: [Predicate] -> [Constraint Kind]
--       infer'context context = concatMapM infer'predicate context

--       infer'predicate :: Predicate -> Infer [Constraint Kind]
--       infer'predicate (Is'In cl'name type') = do
--         (k, k'cs) <- infer'type type'

--         constraint'env <- asks constraint'env

--         param'kind <- case constraint'env Map.!? cl'name of
--           Nothing -> throwError $ Internal "Unexpected: While doing a kind inference I have found a Predicate which is not registered in the Constraint Environment."
--           Just kind -> return kind

--         return (k `Unify` param'kind : k'cs)
--       {- END OF NEW -}


--       infer'n'unify :: Type -> Infer [Constraint Kind]
--       infer'n'unify t = do
--         (k, k'cs) <- infer'type t
--         return $ (k `Unify` K'Star) : k'cs

--       collect'con :: Constr'Decl -> Infer [Constraint Kind]
--       collect'con (Con'Decl _ types) = do
--         -- kinds <- foldM infer'many [] types
--         concatMapM infer'n'unify types

--       collect'con (Con'Record'Decl _ fields) = do
--         concatMapM infer'n'unify $ map snd fields

-- -- TODO: go over all the Data declarations
-- -- don't forget to create new constraints
-- -- infer Kinds from every Type annotation in the program - that means, going over the function bindings too?
-- --    maybe I don't need to? since I've already been there? But did I do collect the important constraints?
-- -- TODO: check the Frea's codebase
