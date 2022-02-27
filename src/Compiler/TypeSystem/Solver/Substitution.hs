module Compiler.TypeSystem.Solver.Substitution where


import qualified Data.Map.Strict as Map



-- | Substitution -- ordered mapping between a name `k` and a value `v`
newtype Subst k v = Sub (Map.Map k v)
  deriving (Eq, Show)


empty'subst :: Subst k a
empty'subst = Sub Map.empty
