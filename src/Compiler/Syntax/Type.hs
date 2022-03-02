{-# LANGUAGE FlexibleContexts #-}

module Compiler.Syntax.Type where


import Data.List ( intercalate )
import qualified Data.Map.Strict as Map
import Control.Monad.State ( MonadState )


import Compiler.Counter ( real'fresh, Counter )

import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Qualified ( Qualified(..) )

import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )
import Compiler.TypeSystem.Infer (Infer)
import Compiler.TypeSystem.Error ( Error )
import Compiler.TypeSystem.Solver ( run'solve )
import Compiler.TypeSystem.Constraint ( Constraint(Match) )


type Sigma'Type = Type


type Rho'Type = Type


type Tau'Type = Type


data T'V = T'V Name Kind
  deriving (Eq)


instance Show T'V where
  show (T'V name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


{- NOTE: this is SOLELY because Map T'V _ and it's `Map.findWithDefault` operation -}
{-  The problem is - the T'V contains a Kind value and it must not be taken into an account
    when looking up the variables in the process of substitution.
    Otherwise it would be thrown off by different Kind Variables representing the same thing.
    Maybe I will figure out a way, how to get rid of this problem and then I should be able to refactor this back to deriving. -}
{- NOTE: this is SOLELY because Map T'V _ and it's `Map.union` operation -}
-- Maybe this isn't really necessary, deriving (Ord) may also work
instance Ord T'V where
  (T'V n'l _) <= (T'V n'r _) = n'l <= n'r


data T'C = T'C Name Kind
  deriving (Eq)


instance Show T'C where
  show (T'C name kind) = "(" ++ name ++ " :: " ++ show kind ++ ")"


data Type
  = T'Var T'V
  | T'Con T'C
  | T'Tuple [Type]
  | T'App Type Type
  | T'Forall [T'V] (Qualified Type)
  deriving (Eq)


{-
For now, I am not including representation for Type Synonyms.
I suppose they should be represented as type declarations.
In the most simple form, something like (Name, [Name], Type).

With that, semantic analysis must make sure that all type synonyms are fully applied.
I can also collect constraints before alias substitution - just to get possibly little better error messages.

Although it would be interesting to lift this restriction same as in Frea and see what exactly is lost,
from type safety standpoint. And how far we can get to the cliff before falling.
 -}


instance Show Type where
  show (T'Var (T'V name kind'))
    = name -- ignoring the kind of the type variable
  show (T'Con (T'C name kind'))
    = name -- ignoring the kind of the type constant
  show (T'Tuple types)
    = "(" ++ intercalate ", " (map show types) ++ ")"

  -- (a -> b) -> c ... -> (-> a b) c ... (-> ((-> a) b)) c
  show (T'App (T'App (T'Con (T'C "(->)" _)) t'left@(T'App (T'App (T'Con (T'C "(->)" _)) _) _)) t'right)
    = "(" ++ show t'left ++ ") -> " ++ show t'right
  -- a -> b ... -> a b ... (-> a) b
  show (T'App (T'App (T'Con (T'C "(->)" _)) t'left) t'right)
    = show t'left ++ " -> " ++ show t'right

  show (T'App t'left t'right@(T'App _ _))
    = show t'left ++ " (" ++ show t'right ++ ")"
  show (T'App t'left t'right)
    = show t'left ++ " " ++ show t'right

  show (T'Forall tvs qual'type)
    = "(forall " ++ unwords (map show tvs) ++ " . " ++ show qual'type ++ ")"


{-  SHALLOW SUBSUMPTION -}
-- sh :: Type -> Type -> Infer Bool
sh :: MonadState Counter m => Type -> Type -> m Bool
s@(T'Forall tvs'l q't'l@(ctxt'l :=> t'l)) `sh` (T'Forall tvs'r q't'r) = do   --  SKOL + SPEC + MONO
  --  co je potreba:
  --  nejdriv musim instanciovat ten typ napravo - nahradit vsechny kvantifikovany promeny uplne fresh jmeny
  --  a pak muzu zavolat znova tuhle funkci a skonci to v casu SPEC
  let params = map (\ (T'V name _) -> name) tvs'l

  fresh'strs <- mapM (\ (T'V n k) -> real'fresh params n >>= \ fresh -> return (fresh, k) ) tvs'r

  let ty'vars = map (\ (name, k) -> T'Var (T'V name k)) fresh'strs
      mapping = zip tvs'r ty'vars
      subst   = Sub $ Map.fromList mapping

      (ctxt'r :=> t'r) = apply subst q't'r

  s `sh` t'r
  {-  TODO: momentalne ignoruju kvalifikatory v typu, je to jenom docasny a nesmim to zapomenout opravit.
            problem je, ze ja bych potreboval provest kompletni jednostranou unifikaci celyho (Qualified Type) (matching)
            ale moje Solver infrastruktura neumoznuje vyrobit (Constraint (Qualified Type))
            to by totiz znamenalo vyrobit list constraintu, ktery po vyreseni vyrobi list constraintu jineho druhu
            ---> z Constraint (Qualified Type) by vzniklo nekolik (Constraint Type) a nejspis taky (Constraint Predicate)
            tohle bude komplikovany
            
            mozna by to tady slo vyresit tak, ze rucne vyrobim ty constrainty tady - provedu manualni dekonstrukci qualified'type
            a dostanu constrainty na Type z prave strany :=> a pak contexty budu muset spravne projit a vzdycky svazat ty dva odpovidajici
            na druhou stranu tam bude mozna trosku problem
            protoze kdyz mas kontext jako:
            (Show a, Show b) a druhej (Show d, Show e) - tak neni jasny jak je naparovat
            takze si myslim, ze realne vyresim ten matching jenom na typech a pak tu substituci vezmu a aplikuju ji na ten typ nalevo
            tim by se mely vsechny promenne v Predikatech v kontextu spravne prepsat
            a ja bych pak mel byt schopny uz jenom porovnat dva qualified typy na primou shodu pomoci (==)
            to mi zni jako dobry zaver
  
    -}


  --  jelikoz musim dealovat s predikatama v kontextu
  --  budu muset tu matching substituci vyrobit uz tady
  --  aplikovat ji na oba dva qualified types
  --  a pak porovnat ty
  --  kvuli tomu bude potreba naimplementova korektne Eq pro Qualified

  -- case run'solve' [q't'l `Unify` alpha'renamed'r] :: Either Error (Subst T'V Type) of
  --   Left err -> return False
  --   Right subst -> do
  --     --  uz jenom to, ze jsem tu matching substituci nasel, znamena, ze zleva se jde dostat (pomoci ni) doprava
  --     --  takze tohle je pro me dostatecny na to abych rekl, ze je to subsumption
  --     return True

(T'Forall tvs'l a@(ctxt'l :=> t'l)) `sh` rho = do                         --  SPEC
  --  tady udelam to, ze chci najit matching substitutici zleva doprava
  --  ten typ napravo necham uplne tak jak je
  --  pokud ziskam substituci, aplikuju ji na ten typ nalevo
  --  a zavolam ten posledni MONO case - znova `sh`
  let constraints = [ t'l `Match` rho ]
  
  case run'solve constraints :: Either Error (Subst T'V Type) of
    Left err -> return False
    Right subst -> do
      let t'l'sub = apply subst t'l
      t'l'sub `sh` rho
      
tau'l `sh` tau'r                                          --  MONO
  = return (tau'l == tau'r)
