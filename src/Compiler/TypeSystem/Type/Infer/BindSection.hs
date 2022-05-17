module Compiler.TypeSystem.Type.Infer.BindSection where


import Data.Maybe ( mapMaybe )


import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type (T'Forall) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Overloaded ( Overloaded(..) )

import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'overloads )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Binding ( Explicit(Explicit) )
import Compiler.TypeSystem.BindSection ( Bind'Section )
import Compiler.TypeSystem.Type.Infer.Explicit ( infer'expl )
import Compiler.TypeSystem.Type.Infer.Implicit ( infer'impls )
import Compiler.TypeSystem.Utils.Infer ( merge'into't'env, overload )


{-  NOTE: I think the [Constraint Type] part of the result is not necessary,
    this function is going to solve all the constraints it finds anyway,
    so what is the point in returning them and letting them be solved again (possibly many times) as part of a bigger solution?
-}
infer'bind'section :: Bind'Section -> Type'Check (Bind'Section, [Predicate], [(Name, Sigma'Type)])
infer'bind'section (es, iss) = do
  let as' = [ (v, sc) | Explicit sc Bind'Group{ name = v } <- es ]
  -- register all overloaded explicits into overloaded
  let overloads = mapMaybe (\ (n, T'Forall _ (ctxt :=> _)) -> if null ctxt then Nothing else Just (n, Overloaded)) as'
  -- NOTE: I need to add those overloads to the Infer'State, for the REPL to later use
  add'overloads overloads
  (iss', ps, as'') <- overload overloads $ merge'into't'env as' (infer'seq infer'impls iss)
  results <- merge'into't'env (as' ++ as'') (mapM infer'expl es)
  let preds'  = concat [ preds | (_, preds) <- results ]
      expls'  = [ ex | (ex, _) <- results ]
  return ((expls', iss'), ps ++ preds', as' ++ as'')


infer'seq :: (a -> Type'Check (a, [Predicate], [(Name, Sigma'Type)])) -> [a] -> Type'Check ([a], [Predicate], [(Name, Sigma'Type)])
infer'seq _ [] = return ([], [], [])
infer'seq ti (bs : bss) = do
  (a, ps, as') <- ti bs
  -- NOTE: I need to transform all assumptions into Overloads if their type context is not empty
  let overloads = mapMaybe (\ (n, T'Forall _ (ctxt :=> _)) -> if null ctxt then Nothing else Just (n, Overloaded)) as'
  (as, qs, as'') <- overload overloads $ merge'into't'env as' (infer'seq ti bss)
  return (a : as, ps ++ qs, as' ++ as'')


{-  TODO: Probably change the name and put the function some better place? -}
-- Its purpose is for method type elaboration
check'seq :: (a -> Type'Check (a, [Predicate])) -> [a] -> Type'Check ([a], [Predicate])
check'seq _ [] = return ([], [])
check'seq ti (bs : bss) = do
  (a, ps) <- ti bs
  (as, qs) <- check'seq ti bss
  return (a : as, ps ++ qs)
