module Compiler.TypeSystem.Solver.Occurs where


import Compiler.Syntax
import Compiler.Syntax.Type


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
  name `occurs'in` (T'Var (T'V varname k'))
    = name == varname

  name `occurs'in` (T'Con (T'C conname k'))
    = name == conname -- TODO: So I think I can do that safely. Consider if TyCon didn't exist and everything would just be a TyVar. You would do this check and by the fact that constructors start with upper case letter it wouldn't break anything. 

  name `occurs'in` (T'Tuple ts)
    = any (name `occurs'in`) ts

  name `occurs'in` (T'App left right)
    = name `occurs'in` left || name `occurs'in` right

  -- TODO: / NOTE:  Can this actually ever happen? If the invariant holds, this shouldn't need to be implemented right?
  name `occurs'in` (T'Forall tvs (preds :=> type'))
    = if all (\ (T'V n _) -> name /= n) tvs -- no shadowing
      then name `occurs'in` type' -- NOTE:  This assumes that the type is not ambiguous. Ambiguities must be checked beforehand.
      else False  -- shadowing - so can't occur


instance Occurs Kind where
  name `occurs'in` (K'Var varname)
    = name == varname

  name `occurs'in` K'Star
    = False

  name `occurs'in` (K'Arr left right)
    = name `occurs'in` left || name `occurs'in` right
