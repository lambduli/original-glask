module Compiler.Syntax.Scheme where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Type
import Compiler.Syntax.Qualified


import Compiler.Analysis.TypeSystem.Solver.Substitutable
import Compiler.Analysis.TypeSystem.Solver.Substitution


data Scheme
  = For'All [T'V] (Qualified Type) -- TODO: continue here - refactor according the paper
  -- deriving (Eq)


instance Show Scheme where
  show (For'All [] qual'type')
    = show qual'type'
  show (For'All type'args qual'type')
    = "forall " ++ unwords (map show type'args) ++ " . " ++ show qual'type'


{- TODO: THINK and VALIDATE carefully

    So the question is - how to know if two Type Schemes are equal/equivalent?

    I think they must be the same modulo alpha renaming.
    So if I map every quantified type parameter from the left scheme to it's corresponding counterpart in the right scheme,
    effectivelly creating a substitution assigning each type variable from the left some other type variable from the right,
    and then apply that substitution to the first qualified type, I will get a qualified type, which should be equal to the right one.

    Now - about checking kinds of type variables.
    I should check that those type variables making a pair have the same kind, BUT
    because in this stage of the program, the Kinds might still be a Kind Variables,
    I would need to in fact produce [Constraint Kind] instead of factually checking.
    Operator (==) can't do that.
    So instead I am going to rely on both of the Type Schemes being well formed - if that wasn't the case
    it should be descovered elsewhere.
    
    So the conclusion is: this way it should work.

    Example:

    scheme 1: forall a b c . Show a => a -> b -> c

    scheme 2: forall x y z . Show x => x -> y -> z

    substitution: [ a -> x , b -> y , c -> z ]

    qualified type 1 after substitution: Show x -> x -> y -> z

    That is equal to the qualified type 2.



    Example 2:

    scheme 1: forall a b c . Show a, Eq b => a -> b -> a -> c -> b -> a -> c

    scheme 2: forall c b a . Show c, Eq b => c -> b -> c -> a -> b -> c -> a

    subst: [ a -> c , b -> b , c -> a ]

    qualified type 1 after substitution: Show a, Eq b => a -> b -> a -> c -> b -> a -> c

    They are equal.

    So I think it's worth noting that it really is not a problem to have a substitution which looks like this:
    [ 1 -> x , ... , x -> A ]
    
    The `apply` can be implemented in a way so that all variables are substituted exactly once and "at the same time".
    This would be the approach based on the idea that when the variable is found the whole collection representing the substitution
    is looked up for corresponding mapping.

    BUT if it was implemented in "phases" - like this:
    `apply` iterates over the collection of substitutions (a -> X) one by one and runs the substitution application as many times
    as many associations are in the collection.
    THAT would lead to a potential problem.

-}
instance Eq Scheme where
  For'All vars'l qual't'l == For'All vars'r qual't'r = qual't'r == qual't'l'substituted
    where
      mapping = zip vars'l $ map T'Var vars'r

      sub :: Subst T'V Type
      sub = Sub $ Map.fromList mapping

      qual't'l'substituted = apply sub qual't'l
