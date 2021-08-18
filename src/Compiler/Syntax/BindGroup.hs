module Compiler.Syntax.BindGroup where


import Compiler.Syntax.Name
import Compiler.Syntax.Match



data Bind'Group = Bind'Group { name  :: Name , pats  :: [Match] }
  deriving (Eq, Show)
{- TODO:  Consider adding where clause
          something like where' :: [Declaration] -}
