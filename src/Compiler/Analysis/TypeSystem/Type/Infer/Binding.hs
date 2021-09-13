module Compiler.Analysis.TypeSystem.Type.Infer.Binding where


import Control.Monad.Except


import Compiler.Syntax

import Compiler.Analysis.Error

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Constraint
import Compiler.Analysis.TypeSystem.Binding

import Compiler.Analysis.TypeSystem.Utils.Infer

import Compiler.Analysis.TypeSystem.Solver
import Compiler.Analysis.TypeSystem.Solver.Substitutable

import Compiler.Analysis.TypeSystem.Type.Infer.Match


{-  Description:

    The `infer'expl` function infers and checks a type for the explicitly annotated binding.
    According our strategy - each explicitly annotated binding can be analyzed in isolation.
      All other explicitly typed bindings are going to be registered in the typing context
      and when the explicits are going to be analyzed, all of the implicits
      (non annotated bindings) will be infered and solved.
    This means, that each explicit can be infered and solved immidiately. That is what we are going to do.

-}
infer'expl :: Explicit -> Infer ([Predicate], [Constraint Type], [Constraint Kind])
infer'expl (Explicit scheme bg@Bind'Group{ name = name, alternatives = matches }) = do
  (qs :=> t) <- instantiate scheme
  (preds, cs't, cs'k) <- infer'matches matches t
  -- now solve it
  case run'solve cs't :: Either Error (Subst T'V Type) of
    Left err -> throwError err
    Right subst -> do
      let qs' = apply subst qs
          t'  = apply subst t
          -- so I need to apply the substitution to the typing context
          -- because I need to update 

      undefined
