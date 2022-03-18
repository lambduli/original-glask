module Compiler.TypeSystem.Type.Infer.Literal where


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Literal ( Literal(..) )
import Compiler.Syntax.Predicate ( Predicate(..) )
import Compiler.Syntax.Type ( Rho'Type, Sigma'Type, Type(T'Var', T'Forall, T'Meta), T'V'(T'V') )
import Compiler.Syntax.Qualified ( Qualified(..) )


import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'constraints )
import Compiler.TypeSystem.Type.Constants ( t'Char )
import Compiler.TypeSystem.Expected ( Expected )
import Compiler.TypeSystem.Actual ( Actual )
import Compiler.TypeSystem.Constraint ( Constraint )
import Compiler.TypeSystem.Utils.Infer ( inst'sigma )


-- the future implementation will be calling inst'sigma because of the checking mode
-- and it seems to me, that what if I have something like this:
-- 23 :: Int
-- that leads to the checking mode and literal 23 is checked against the Int
-- so inst'sigma is called with the type of the numeric literal
-- which is forall a . Num a => a
-- and the expected type is Int
-- inst'sigma in checking mode does calls subs'check'rho
-- 
infer'lit :: Literal -> Expected Rho'Type -> Type'Check ([Predicate], Actual Rho'Type)
infer'lit (Lit'Int _) expected = do
  fresh'name <- fresh -- not necessary, since it will get instantiated anyway, but just to be sure
  let t'v = T'V' fresh'name K'Star
      t'var = T'Var' t'v
      sigma :: Sigma'Type
      sigma = T'Forall [t'v] ([Is'In "Num" t'var] :=> t'var)
  inst'sigma sigma expected

infer'lit (Lit'Double double) expected = do
  fresh'name <- fresh -- not necessary, since it will get instantiated anyway, but just to be sure
  let t'v = T'V' fresh'name K'Star
      t'var = T'Var' t'v
      sigma :: Sigma'Type
      sigma = T'Forall [t'v] ([Is'In "Fractional" t'var] :=> t'var)
  inst'sigma sigma expected

infer'lit (Lit'Char char) expected = do
  inst'sigma t'Char expected
  