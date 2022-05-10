module Compiler.TypeSystem.Kind.Infer.Instance where


import qualified Data.Set as Set


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Instance ( Instance )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Type ( T'V'(T'V') )

import Compiler.TypeSystem.Infer ( Kind'Check )
import Compiler.TypeSystem.Kind.Infer.Type ( infer'context, infer'predicate )

import Compiler.TypeSystem.Solver.Substitutable ( Term(free'vars) )
import Compiler.TypeSystem.Utils.Infer (merge'into'k'env)


infer'instance :: Instance -> Kind'Check ()
infer'instance (context :=> pred) = do
  let free'in'inst  = Set.toList $ free'vars $ pred : context :: [T'V']
  -- NOTE: I am assuming that the instance is valid, that means, free variables in context also exist within the predicate
  let assumps       = map (\ (T'V' name kind) -> (name, kind)) free'in'inst

  merge'into'k'env assumps (infer'context context)
  merge'into'k'env assumps (infer'predicate pred)
