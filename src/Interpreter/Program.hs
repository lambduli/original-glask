module Interpreter.Program where


import qualified Data.Map.Strict as Map
import Data.Foldable ( find )


import Compiler.Syntax.BindGroup ( Bind'Group )
import Compiler.Syntax.Declaration ( Declaration, Data, Class )
import Compiler.Syntax.HasKind ( HasKind(kind) )
import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, T'V(..), Type(..) )

import Compiler.TypeSystem.Program ( Program(..) )
import Compiler.TypeSystem.Binding ( Explicit(Explicit), Implicit(Implicit), Method(..) )
import Compiler.TypeSystem.TypeSection ( Type'Section )

import Compiler.TypeSystem.Solver.Substitution ( Subst(..) )
import Compiler.TypeSystem.Solver.Substitutable ( Substitutable(apply) )


import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Syntactic.Data as Data
import qualified Compiler.Analysis.Syntactic.Class as Classes

import qualified Compiler.Analysis.Semantic.Dependency.Binding as Bindings
import qualified Compiler.Analysis.Semantic.Dependency.Types as Types



to'program :: [Declaration] -> Program
to'program decls = Program{ bind'sections = [(explicits, implicits)], methods = methods, method'annotations = m'anns, data'n'class'sections = type'sections {- data'declarations = data'decls -} }
  where
    method'annotations :: [(Name, Sigma'Type, Name)]
    method'annotations = Method'Annotations.extract decls

    m'anns = map (\ (method'n, s't, _) -> (method'n, s't)) method'annotations

    method'bindings :: [(Name, Bind'Group, Type)]
    method'bindings = Method'Bindings.extract decls

    {-  NOTE:  -}
    methods :: [Method]
    methods = map make'method method'bindings
      where
        make'method :: (Name, Bind'Group, Type) -> Method
        make'method (method'name, bind'group, instance'type) =
          let Just (_, T'Forall tvs qualified'type, class'var'name) = find (\ (m'n, _, _) -> m'n == method'name) method'annotations -- NOTE: This should always find the result, so the pattern matching on Just (...) should always succeed
              substitution :: Subst T'V Type
              substitution = Sub (Map.singleton (T'V class'var'name (kind instance'type)) instance'type) -- NOTE: the Type Variable must have the same Kind as the Instance Type
              scheme = T'Forall (filter (\ (T'V n _) -> n /= class'var'name) tvs ) $ apply substitution qualified'type -- now I have the Type Scheme
          in Method scheme bind'group

 

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

    d'n'c'secs   :: [[Either Data Class]]
    d'n'c'secs   = Types.sort (map Left data'decls ++ map Right class'decls)

    type'sections = map to'type'section d'n'c'secs

    to'type'section :: [Either Data Class] -> Type'Section
    to'type'section eithers =
      let ds = [ d | Left d <- eithers ]
          cs = [ c | Right c <- eithers ]
      in  (ds, cs)
