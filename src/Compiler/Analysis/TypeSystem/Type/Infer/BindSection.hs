module Compiler.Analysis.TypeSystem.Type.Infer.BindSection where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Binding
import Compiler.Analysis.TypeSystem.BindSection

import Compiler.Analysis.TypeSystem.Type.Infer.Explicit
import Compiler.Analysis.TypeSystem.Type.Infer.Implicit

import Compiler.Analysis.TypeSystem.Utils.Infer



{-  NOTE: I think the [Constraint Type] part of the result is not necessary,
    this function is going to solve all the constraints it finds anyway,
    so what is the point in returning them and letting them be solved again (possibly many times) as part of a bigger solution?
-}
infer'bind'section :: Bind'Section -> Infer ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
infer'bind'section (es, iss) = do
  let as' = [ (v, sc) | Explicit sc Bind'Group{ name = v } <- es ]
  (ps, as'', cs't, cs'k) <- merge'into't'env as' (infer'seq infer'impls iss)
  results <- merge'into't'env (as' ++ as'') (mapM infer'expl es)
  let preds'  = concat [ preds  | (preds, _   , _   ) <- results ]
      cs't'   = concat [ cs't   | (_    , cs't, _   ) <- results ]
      cs'k'   = concat [ cs'k   | (_    , _   , cs'k) <- results ]
  return (ps ++ preds', as' ++ as'', cs't ++ cs't', cs'k ++ cs'k')


-- infer'seq :: ([Implicit] -> Infer ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])) -> [[Implicit]] -> Infer ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
infer'seq :: (a -> Infer ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])) -> [a] -> Infer ([Predicate], [(Name, Scheme)], [Constraint Type], [Constraint Kind])
infer'seq _ [] = return ([], [], [], [])
infer'seq ti (bs : bss) = do
  (ps, as', cs't, cs'k) <- ti bs
  (qs, as'', cs't', cs'k') <- merge'into't'env as' (infer'seq ti bss)
  return (ps ++ qs, as' ++ as'', cs't ++ cs't', cs'k ++ cs'k')
