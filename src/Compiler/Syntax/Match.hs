module Compiler.Syntax.Match where


import Data.List

import Compiler.Syntax.Pattern
import {-# SOURCE #-} Compiler.Syntax.Expression


{- NOTE: I need to find out why exactly is the Match'Context needed.
  From one standpoint I could need it to serialize/pretty print, but do I really?
  It seems to me, that I should be able to figure out the context
  from the constructor of Expression or Term which is holding the value of the type Match'Group.
  And I shouldn't use simple `show` for printing anyway!
  So additional arguments like context and indentation are possible.

  But then again, if I implement it like some kind of type class,
  then I probably won't pass context in every instance of the serialization
  - for matching unrelated parts of the language I mean.
  So maybe keeping the context in the data type isn't bad idea after all.

  But should I keep it in the Match'Group?
  Because to correctly intercalate the Match'es it would be needed.
  But it would also be needed in the Match itself.
  Would it be wrong to keep it in both of them?

  So then it could actually be type parameter right?
  Match'Group In'Declaration and Match In'Declaration vs
  Match'Group In'Case and Match In'Case.

  So maybe if I discover that I don't really need it represented as a value
  I could represent it on the type level.

  I can also implement showList for Match
  and check that all contexts are same and then intercalate according to that.
 -}


{- QUESTION:
    Why is there a list of patterns and not a single Pattern
    then the rhs would possibly contain a lambda abstraction.
    Or many nested.

    The current situation is - I have the head of the declaration as an expression.
    That means - it's an Application Term containing a list of Expression Terms.
    That Application Term needs to be translated to the Pattern Application Term.
    And it also means the correct association must be decided using Shunting Yard Algorithm.
    This step will then produce Pattern AST. And that is in the form of a tree.
    So producing a list of Patterns seems counter productive and counter intuitive.

    Sat 20.11.2021
    What is important is how do I infer the bindings?
    And it seems to work on list of patterns and not a pattern tree or a tree of nested lambdas.
    So I will probably keep the shape of the Match with `patterns` being `[Pattern]`
 -}


data Match
  = Match { patterns :: [Pattern]
          , rhs :: Expression
        {-, ctxt :: Match'Context -}
        {-, wheres :: [Declaration] -}
        {-, guards :: [Guard] -} }
  deriving (Eq, Show)


-- data Match'Context
--   = M'Declaration
--   | M'Case
--   deriving (Show, Eq)
