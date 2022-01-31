module Compiler.Analysis.TypeSystem.Program where


import Compiler.Analysis.TypeSystem.BindSection (Bind'Section)
import Compiler.Analysis.TypeSystem.Binding


{-  NOTE: I am looking at the definition of `Program` and thinking.
          It seems like this is yet another representation - very similar to what I already have,
          but specific in information I would be carrying around.
          I obviously appreciate not having to carry the whole list of Declarations.
          I also realize I will need to constantly build these "specific" representations.
          I will also need to maintain all of the code created to specify them.
          I should constantly be weighting the pros and cons.
          If there's more cons to have it be "specific"
          I should just make that part of the system work with the previous and already-defined representation.
-}

data Program = Program{ bind'sections :: [Bind'Section], methods :: [Method] }
  deriving (Show)
