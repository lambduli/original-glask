module Interpreter.Program where


import qualified Data.Map.Strict as Map
import Data.Foldable


import Compiler.Syntax
import Compiler.Syntax.Type

import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Binding

import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable


import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Syntactic.Data as Data

import qualified Compiler.Analysis.Semantic.DependencyAnalysis as Dependencies
import qualified Compiler.Analysis.Semantic.Class as Classes
import Compiler.TypeSystem.Utils.Infer



to'program :: [Declaration] -> Program
to'program decls = Program{ bind'sections = [(explicits, implicits)], methods = methods, method'annotations = m'anns, data'declarations = data'decls }
  where
    method'annotations :: [(Name, Qualified Type, Name)]
    method'annotations = Method'Annotations.extract decls

    m'anns = map (\ (method'n, q't, _) -> (method'n, q't)) method'annotations

    method'bindings :: [(Name, Bind'Group, Type)]
    method'bindings = Method'Bindings.extract decls

    {-  NOTE:  -}
    methods :: [Method]
    methods = map make'method method'bindings
      where
        make'method :: (Name, Bind'Group, Type) -> Method
        make'method (method'name, bind'group, instance'type) =
          let Just (_, qualified'type, class'var'name) = find (\ (m'n, _, _) -> m'n == method'name) method'annotations -- NOTE: This should always find the result, so the pattern matching on Just (...) should always succeed
              substitution :: Subst T'V Type
              substitution = Sub (Map.singleton (T'V class'var'name (kind instance'type)) instance'type) -- NOTE: the Type Variable must have the same Kind as the Instance Type
              scheme = close'over $ apply substitution qualified'type -- now I have the Type Scheme
          in Method scheme bind'group

 

    annotations :: Map.Map Name (Qualified Type)
    annotations = Annotations.extract decls

    bindings :: Map.Map Name Bind'Group
    bindings = Bindings.extract decls -- NOTE: Myslim, ze tohle jde volat jenom tehdy, kdyz uz jsou vsechny Bind Groups mergnuty do jedne - pokud maji stejne jmeno.

    explicit'map :: Map.Map Name (Qualified Type, Bind'Group)
    explicit'map = Map.intersectionWith (,) annotations bindings

    implicit'map :: Map.Map Name Bind'Group
    implicit'map = Map.difference bindings explicit'map

    explicits = map (\ (q't, b'g) -> Explicit (close'over q't) b'g) $ Map.elems explicit'map

    implicits = map (map Implicit) $ Dependencies.sort $ Map.elems implicit'map

    data'decls = Data.extract decls
