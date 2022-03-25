module Compiler.TypeSystem.Type.Infer.BindSection where


import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type )

import Compiler.TypeSystem.Infer ( Infer, Type'Check )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Binding ( Explicit(Explicit) )
import Compiler.TypeSystem.BindSection ( Bind'Section )
import Compiler.TypeSystem.Type.Infer.Explicit ( infer'expl )
import Compiler.TypeSystem.Type.Infer.Implicit ( infer'impls )
import Compiler.TypeSystem.Utils.Infer ( merge'into't'env )


{-  NOTE: I think the [Constraint Type] part of the result is not necessary,
    this function is going to solve all the constraints it finds anyway,
    so what is the point in returning them and letting them be solved again (possibly many times) as part of a bigger solution?
-}
infer'bind'section :: Bind'Section -> Type'Check ([Predicate], [(Name, Sigma'Type)])
infer'bind'section (es, iss) = do
  let as' = [ (v, sc) | Explicit sc Bind'Group{ name = v } <- es ]
  (ps, as'') <- merge'into't'env as' (infer'seq infer'impls iss)
  results <- merge'into't'env (as' ++ as'') (mapM infer'expl es)
  let preds'  = concat results
  return (ps ++ preds', as' ++ as'')


infer'seq :: (a -> Type'Check ([Predicate], [(Name, Sigma'Type)])) -> [a] -> Type'Check ([Predicate], [(Name, Sigma'Type)])
infer'seq _ [] = return ([], [])
infer'seq ti (bs : bss) = do
  (ps, as') <- ti bs
  (qs, as'') <- merge'into't'env as' (infer'seq ti bss)
  return (ps ++ qs, as' ++ as'')


{-  TODO: Probably change the name and put the function some better place? -}
-- Its purpose is for method type elaboration
check'seq :: (a -> Type'Check [Predicate]) -> [a] -> Type'Check [Predicate]
check'seq _ [] = return []
check'seq ti (bs : bss) = do
  ps <- ti bs
  qs <- check'seq ti bss
  return $ ps ++ qs
