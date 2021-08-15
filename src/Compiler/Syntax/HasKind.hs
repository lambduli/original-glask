module Compiler.Syntax.HasKind where


import Compiler.Syntax.Kind
import Compiler.Syntax.Type


class HasKind t where
  kind :: t -> Kind
  -- NOTE: maybe make it return (Maybe Kind)


instance HasKind T'V where
  kind (T'V _ k) = k


instance HasKind T'C where
  kind (T'C _ k) = k


{- following definition is only partial function, it is assumed that the types are always well formed -}
instance HasKind Type where
  kind (T'Var tv) = kind tv
  kind (T'Con tcon) = kind tcon
  kind (T'Tuple _) = K'Star -- assuming the type is well formed
  kind (T'App t _)
    = case kind t of
      K'Arr _ k -> k
      -- assuming the type is well formed, therefore there's no other option
