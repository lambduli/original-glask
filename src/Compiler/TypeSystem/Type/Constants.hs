module Compiler.TypeSystem.Type.Constants where


import Compiler.Syntax.Type
import Compiler.Syntax.Kind


t'Bool, t'Int, t'Double, t'Char, t'Arr :: Type
-- | Assuming that Bool will be defined in the Prelude.
t'Bool = T'Con $ T'C "Bool" K'Star

t'Int = T'Con $ T'C "Int" K'Star

t'Double = T'Con $ T'C "Double" K'Star

t'Char = T'Con $ T'C "Char" K'Star

t'Arr = T'Con $ T'C "(->)" (K'Star `K'Arr` (K'Star `K'Arr` K'Star))


type'list :: Type
type'list = T'Con $ T'C "[]" (K'Star `K'Arr` K'Star)


infixr 4 `type'fn`
type'fn :: Type -> Type -> Type
domain `type'fn` codomain = T'App (T'App t'Arr domain) codomain
