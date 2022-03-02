module Compiler.TypeSystem.Type.Infer.BindSection where


import Compiler.Syntax.BindGroup ( Bind'Group(Bind'Group, name) )
import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Predicate ( Predicate )
import {-# SOURCE #-} Compiler.Syntax.Type ( Sigma'Type, Type )

import Compiler.TypeSystem.Infer ( Infer )
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
infer'bind'section :: Bind'Section -> Infer ([Predicate], [(Name, Sigma'Type)], [Constraint Type])
infer'bind'section (es, iss) = do
  let as' = [ (v, sc) | Explicit sc Bind'Group{ name = v } <- es ]
  (ps, as'', cs't) <- merge'into't'env as' (infer'seq infer'impls iss)
  results <- merge'into't'env (as' ++ as'') (mapM infer'expl es)
  let preds'  = concat [ preds  | (preds, _   ) <- results ]
      cs't'   = concat [ cs't   | (_    , cs't) <- results ]
  return (ps ++ preds', as' ++ as'', cs't ++ cs't')


infer'seq :: (a -> Infer ([Predicate], [(Name, Sigma'Type)], [Constraint Type])) -> [a] -> Infer ([Predicate], [(Name, Sigma'Type)], [Constraint Type])
infer'seq _ [] = return ([], [], [])
infer'seq ti (bs : bss) = do
  (ps, as', cs't) <- ti bs
  (qs, as'', cs't') <- merge'into't'env as' (infer'seq ti bss)
  return (ps ++ qs, as' ++ as'', cs't ++ cs't')


{-  TODO: Probably change the name and put the function some better place? -}
-- Its purpose is for method type elaboration
check'seq :: (a -> Infer ([Predicate], [Constraint Type])) -> [a] -> Infer ([Predicate], [Constraint Type])
check'seq _ [] = return ([], [])
check'seq ti (bs : bss) = do
  (ps, cs't) <- ti bs
  (qs, cs't') <- check'seq ti bss
  return (ps ++ qs, cs't ++ cs't')
