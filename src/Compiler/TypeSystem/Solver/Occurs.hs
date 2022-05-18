module Compiler.TypeSystem.Solver.Occurs where


import Compiler.Syntax.Kind ( Kind(..) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import {-# SOURCE #-} Compiler.Syntax.Type ( T'C(T'C), T'V'(T'V'), Type(..), M'V(..) )


class Occurs a where
  occurs'in :: String -> a -> Bool


{- NOTE on the NOTE: The following note is taken directly from the Frea implementation. -}
{- NOTE:  This piece of code raises a question.
          Is it possible to replace the whole Occurable class with it's single method `occurs'in`
          with just calling free'vars of Term and then testing whether your thing is the member of the set?

          It would seem that depends on whether that specific behaviour for the TyCon is used and important.
          I *think* it is used for type synonym expansion now. (Type synonym is tested for presence in the type expression and so on.)
          But that doesn't necessary mean it has to stay. I will need to refactor type synonym implementation in the future.
          Since mine allows for more expressive type expressions to be constructed (partially applied type synonyms / functions).
          I as we know, this has a potential to break decidability. So I will get rid of it and proceed same as GHC.
-}
instance Occurs Type where
  name `occurs'in` (T'Var' _)
    = False
  
  name `occurs'in` (T'Meta (Tau varname k'))
    = name == varname

  name `occurs'in` (T'Con _)
    = False

  name `occurs'in` (T'Tuple ts)
    = any (name `occurs'in`) ts

  name `occurs'in` (T'App left right)
    = name `occurs'in` left || name `occurs'in` right

  -- TODO: / NOTE:  Can this actually ever happen? If the invariant holds, this shouldn't need to be implemented right?
  name `occurs'in` (T'Forall _ (preds :=> type'))
  -- INVARIANT: If the type is well formed, any variable within the context must also be in the type' too and also quantified by the forall
    = name `occurs'in` type'


instance Occurs Kind where
  name `occurs'in` (K'Var varname)
    = name == varname

  name `occurs'in` K'Star
    = False

  name `occurs'in` (K'Arr left right)
    = name `occurs'in` left || name `occurs'in` right
