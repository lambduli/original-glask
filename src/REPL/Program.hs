module REPL.Program where


import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Foldable ( find )
import Data.List ( deleteBy )
import Control.Monad.State


import Compiler.Counter ( real'fresh, Counter(..) )

import Compiler.Syntax.BindGroup ( Bind'Group )
import Compiler.Syntax.Declaration ( Declaration, Data, Class )
import Compiler.Syntax.HasKind ( HasKind(kind) )
import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V'(..), Type(..), M'V (Tau) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Predicate ( Predicate(Is'In) )

import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Implicit(Implicit), Method(..) )
import Compiler.TypeSystem.TypeSection ( Type'Section )

import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply), Term (free'vars) )


import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Syntactic.Data as Data
import qualified Compiler.Analysis.Syntactic.Class as Classes
import qualified Compiler.Analysis.Syntactic.Instance as Instances

import qualified Compiler.Analysis.Semantic.Dependency.Binding as Bindings
import qualified Compiler.Analysis.Semantic.Dependency.Types as Types


to'program :: [Declaration] -> Program
to'program decls = Program{ bind'section = (explicits, implicits), methods = methods, method'annotations = m'anns, data'n'class'sections = type'sections {- data'declarations = data'decls -} }
  where
    method'annotations :: [(Name, Sigma'Type, Name, Name)]
    method'annotations = Method'Annotations.extract decls

    m'anns = map (\ (method'n, s't, _, cl'name) -> (method'n, s't, cl'name)) method'annotations

    method'bindings :: [(Name, Bind'Group, Type, Name)] -- method name, bg, instance type, class name
    method'bindings = Method'Bindings.extract decls

    {-  NOTE:  -}
    methods :: [Method]
    methods = map make'method method'bindings
      where
        make'method :: (Name, Bind'Group, Type, Name) -> Method
        make'method x@(method'name, bind'group, instance'type, class'name) =
          let Just (_, T'Forall tvs qualified'type, class'var'name, cl'name) = find (\ (m'n, _, _, _) -> m'n == method'name) method'annotations -- NOTE: This should always find the result, so the pattern matching on Just (...) should always succeed
              Just cl'param'tv = find (\ (T'V' n _) -> n == class'var'name) tvs  -- (T'V' class'var'name (kind instance'type))

              free'in'inst'type = Set.toList $ free'vars instance'type :: [T'V'] -- I want free rigid type variables
              taken'names       = map (\ (T'V' n _) -> n) tvs
              (unique'names, _) = runState (mapM (real'fresh taken'names) free'in'inst'type) (Counter{ counter = 0})
              -- NOTE/TODO: I know this is ugly, but I am just trying to quick-fix it
              -- TODO: find a better solution please
              -- This is about quanlified instances and instance method impls in general
              mapping           = map (\ (name, T'V' n k) -> (T'V' n k, T'Var' $ T'V' name k)) $ zip unique'names free'in'inst'type
              uniq'subst        = Sub (Map.fromList mapping)
              instance'type'    = apply uniq'subst instance'type
              tvs'              = map (\ (name, T'V' n k) -> T'V' name k) $ zip unique'names free'in'inst'type

              
              substitution :: Subst T'V' Type
              substitution = Sub (Map.singleton cl'param'tv instance'type') -- NOTE: the Type Variable must have the same Kind as the Instance Type
              (orig'ctxt :=> orig'type) = qualified'type
              striped = (filter (\ (Is'In c'n t) -> c'n /= class'name) orig'ctxt) :=> orig'type -- TODO: This is super dirty trick, I should be able to do it, because method annotations will
              -- only have one predicate with the name of the class like foo :: Foo a , ... => ...
              -- because such predicate is not legal to be written by the user, it will be the one I have put here
              -- so I can now remove it
              -- later I should just not put it here, so I don't have to remove it
              q't = striped
              (apl'ctxt :=> apl't) = apply substitution q't
              q'ty = (apl'ctxt :=> apl't)
              -- q'ty = (filter (\ (Is'In c'name t) -> ) apl'ctxt) :=> apl't
              scheme = T'Forall ((deleteBy (\ (T'V' n _) (T'V' n' _) -> n == n') (T'V' class'var'name undefined) tvs) ++ tvs') q'ty -- now I have the Type Scheme
              -- ^^^ I know the use of undefined is not a good idea, BUT I really wanted to emphasise that I only want to delete single type variable from the generic variables
              -- (filter (\ (T'V' n _) -> n /= class'var'name) tvs ) 

              -- I actually need to consider that the `instance'type` might contain some type variables too
              -- so first I need to give them unique name in regards to the rest of the names in the `tvs`
              -- then I alpha rename them in the instance'type
              -- after that I finally have a correct type which can be substituted for the `cl'param'tv`
              -- and then when the `scheme` is being created I can simply add those free type variables which I have just alpha renamed to unique names
              -- into the forall, after I remove

              method = Method scheme bind'group class'name

          in method

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


    data'decls    :: [Data]
    data'decls    = Data.extract decls

    class'decls   :: [Class]
    class'decls   = Classes.extract decls

    is            = Instances.extract decls

    d'n'c'secs    :: [[Either Data Class]]
    d'n'c'secs    = Types.sort (map Left data'decls ++ map Right class'decls)

    type'sections = map to'type'section d'n'c'secs

    to'type'section :: [Either Data Class] -> Type'Section
    to'type'section eithers =
      let ds = [ d | Left d <- eithers ]
          cs = [ c | Right c <- eithers ]
      in  (ds, cs, is)
