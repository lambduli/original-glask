module Compiler.TypeSystem.Utils.Class where


import qualified Data.Map.Strict as Map
import Data.Maybe
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Trans.Except (catchE)


import Compiler.Syntax

import Compiler.TypeSystem.Error
import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable
import Compiler.TypeSystem.InferenceEnv
import Compiler.TypeSystem.Class
import Compiler.TypeSystem.Solver.Solve
import Compiler.TypeSystem.Solver.Unify
import Compiler.TypeSystem.Type.Constants


{- TODO: NOTE: it's partial, I don't quite like that. Fix that later. -}
super :: Class'Env -> Name -> [Name]
super cl'env name
  = case classes cl'env Map.!? name of
      Just (supers, instances) -> supers
      _ -> error "super is still a partial function"


-- Question: So what if there are no instances for that particular class? Can I just return an empty list?
-- 31.1.2022 NOTE: It seems like that's what I should return.
instances :: Class'Env -> Name -> [Instance]
instances cl'env name
  = case classes cl'env Map.!? name of
      Just (supers, instances) -> instances
      Nothing -> [] -- NOTE: This is just temporary, I don't know if I can do this, just checking how things work.
      _ -> error "instances is still a partial function"


modify :: Class'Env -> Name -> Class -> Class'Env
modify cl'env@Class'Env{ classes = classes } name class'
  = cl'env{ classes = Map.insert name class' classes }


initial'env :: Class'Env
initial'env = Class'Env { classes = Map.empty, defaults = [t'Int, t'Double] }

type EnvTransformer = Class'Env -> Solve Class'Env


infixr 5 <:>
(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(f <:> g) cl'env = do
  cl'env' <- f cl'env
  g cl'env'



-- TODO: REWRITE THE RESULT TYPE
-- following three functions will return something like Solve (...)
add'class :: Name -> [Name] -> EnvTransformer
add'class name names cl'env
  | isJust (classes cl'env Map.!? name) = throwError $ Unexpected "class already exists"
  | any (isNothing . (\ name -> classes cl'env Map.!? name)) names = throwError $ Unexpected "superclass not defined"
  | otherwise = return (modify cl'env name (names, []))


add'inst :: [Predicate] -> Predicate -> EnvTransformer
add'inst preds pred@(Is'In name _) cl'env
  | isNothing $ classes cl'env Map.!? name = throwError $ Unexpected "no class for instance"
  | otherwise = do
      let overlapping = anyM (overlap pred) qs
      ifM overlapping (throwError $ Unexpected "overlapping instance") (return $ modify cl'env name c)
        where
          its = instances cl'env name
          qs = [q | (_ :=> q) <- its]
          c = (super cl'env name, (preds :=> pred) : its)
-- TODO: rename most of the stuff in this function so it's aparent what is going on here


{- TODO:  fix PLS!
          What is wrong with it:  `overlap` returns Bool, but actually it relies on the Error to signalize "overlapping".
                                  In that case, it would make more sense to just return (). But then I need to use something different from `ifM`.
          What would be better: 
 -}
overlap :: Predicate -> Predicate -> Solve Bool
overlap p q = do
  p `unify` q :: Solve (Subst T'V Type)
  return False
-- takze co se tady deje
-- moje unify nevraci Maybe (Subst ...)
-- ja to mam schovany v monad transformeru abych mohl reportovat ruzny konkretni duvody, proc neslo unifikovat
-- coz je ofc lepsi, nez jenom Maybe a rict - Nothing
-- takze logicky, i tahle funkce, bude muset pracovat s mym monad transformerem
-- a tim padem i vsechny ostatni - prirozene
-- ono ostatne to dava smysl - stejne jsou to funkce, ktery budu volat kdyz uz budu uvnitr monad transf




by'super :: Class'Env -> Predicate -> [Predicate]
by'super cl'env pred@(Is'In name type')
  = pred : concat [by'super cl'env (Is'In name' type') | name' <- super cl'env name]


by'inst :: Class'Env -> Predicate -> Solve [Predicate]
by'inst cl'env pred@(Is'In name type') =
  let insts = instances cl'env name
  in first'defined insts
  
  -- first'defined [try'inst it | it <- instances cl'env name]
  where
      try'inst :: Instance -> Solve [Predicate]
      try'inst (preds :=> head) = do
        u <- match head pred :: Solve (Subst T'V Type)
        return (map (apply u) preds)

      first'defined :: [Instance] -> Solve [Predicate]
      first'defined [] = throwError $ Unexpected "Error: I think I didn't find any instances of the thing."
      first'defined (inst : insts) = do
        -- v <- try'inst inst -- NOTE: Tohle jsem zakomentoval hodne pozde v noci. Myslim, ze `v` se nikde nepouziva
        -- a ze to vzniklo z chyby
        -- myslim, ze je to urcite spatne, protoze kdyz by nahodou `try'inst inst` zpusobylo error
        -- tak tim, ze je tady uplne nechranenej, tak to proste failne
        -- tusim, ze jsem tohle psal tesne pred nebo na Petrove prednasce z PPA nekdy zkraje semestru
        -- nejspis jsem nedaval dost pozor
        -- TODO: Make sure, that it is - in fact - correct, to delete that line.
        -- TODO: Predtim, nez to smazu, tak me zajima proc se to chova tak, ze kdyz tahle radka tady je,
        -- tak zalezi na poradi definice instance pro Num (Int a Double) pro priklad `flop`
        -- to je skutecne zajimavy
        -- mam nejakou teorii, ze to je proto, ze tohle sice bouchne, ale v pripade Implicitniho `flopu` je ta chyba osetrena
        -- nebo spis naopak
        -- a vyradi to jeden potencialni defaulting
        -- je divny ale, ze to vyradi jeden z nich (ten prvni? nebo ne?) ale pro ten druhej to uz nebouchne? nebo proc ho to nevyradi?
        -- zvlastni! 
        catchE (try'inst inst) (const $ first'defined insts)


entail :: Class'Env -> [Predicate] -> Predicate -> Solve Bool
entail cl'env preds pred = do
  let is'by'super = any (pred `elem`) (map (by'super cl'env) preds)
  either'is'by'inst'or'not <- tryE (by'inst cl'env pred)
  let is'by'inst = case either'is'by'inst'or'not of
                    Left _ -> False
                    Right _ -> True
  return $ is'by'super || is'by'inst



{- NOTE: For some reason this function is not exported from the module it should be exported.
        So until I figure out what's up, I will just copy it here by hand. -}
tryE :: Monad m => ExceptT e m a -> ExceptT e m (Either e a)
tryE m = catchE (liftM Right m) (return . Left)


in'hnf :: Predicate -> Bool
in'hnf (Is'In class'name type') = hnf type'
  where
    hnf (T'Var var) = True
    hnf (T'Con con) = False
    hnf (T'App type'l _) = hnf type'l
    hnf (T'Tuple types) = True
    -- leaving out TyTuple -> I will refactor it out eventually anyway
    -- TODO: I implemented it for now, BUT I am not sure if a Tuple type is in the head normal form
    -- in any case - let's get rid of special case for the Tuple ASAP


to'hnfs :: Class'Env -> [Predicate] -> Solve [Predicate]
to'hnfs cl'env preds = do
  predicates <- mapM (to'hnf cl'env) preds
  return $ concat predicates


to'hnf :: Class'Env -> Predicate -> Solve [Predicate]
to'hnf cl'env pred
  | in'hnf pred = return [pred]
  | otherwise = do
    either'err'or'preds <- tryE $ by'inst cl'env pred
    case either'err'or'preds of
      Left err -> throwError $ Unexpected $ "Failed in context reduction. I think some predicate is unsatisfiable." ++ " | " ++ show err
      Right preds -> to'hnfs cl'env preds


simplify :: Class'Env -> [Predicate] -> Solve [Predicate]
simplify cl'env = loop []
  where
    loop rs [] = return rs
    loop rs (pred : preds) = do
      entailed <- entail cl'env (rs ++ preds) pred
      if entailed
      then loop rs preds
      else loop (pred : rs) preds


reduce :: Class'Env -> [Predicate] -> Solve [Predicate]
reduce cl'env preds = do
  qs <- to'hnfs cl'env preds
  simplify cl'env qs
