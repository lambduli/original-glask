{-# LANGUAGE NoMonomorphismRestriction #-}

module Compiler.TypeSystem.Type.Infer.Expression where


import Control.Monad


import Compiler.Syntax
import Compiler.Syntax.Expression

import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Constraint
import Compiler.TypeSystem.Type.Constants

import Compiler.TypeSystem.Type.Infer.Literal
import Compiler.TypeSystem.Type.Infer.Pattern
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Match
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Declaration
import Compiler.TypeSystem.Utils.Infer


infer'expr :: Expression -> Infer ([Predicate], Type, [Constraint Type], [Constraint Kind])
infer'expr (Var var'name) = do
  preds :=> type' <- lookup't'env var'name
  return (preds, type', [], [])

infer'expr (Const const'name) = do
  preds :=> type' <- lookup't'env const'name
  return (preds, type', [], [])

infer'expr (Op op'name) = do
  preds :=> type' <- lookup't'env op'name
  return (preds, type', [], [])

infer'expr (Lit lit) = do
  (preds, type') <- infer'lit lit
  return (preds, type', [], [])

-- TODO: check if it's really valid
infer'expr (Abs pattern'param body) = do
  (preds'param, type'param, assumptions'param) <- infer'pat pattern'param
  (preds'body, type'body, t'constrs, k'constrs) <- merge'into't'env assumptions'param (infer'expr body)
  return (preds'param ++ preds'body, type'param `type'fn` type'body, t'constrs, k'constrs)

-- TODO: check if it's really valid
infer'expr (App left right) = do
  (preds'l, t'l, t'cs'l, k'cs'l) <- infer'expr left
  (preds'r, t'r, t'cs'r, k'cs'r) <- infer'expr right
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  let (t'c, k'c) = t'l `unify'types` (t'r `type'fn` t'var)
  return (preds'l ++ preds'r, t'var, t'c : t'cs'l ++ t'cs'r, k'c : k'cs'l ++ k'cs'r)

-- TODO: check if it's really valid
infer'expr (Infix'App left op right) = do
  (preds'l, t'l, t'cs'l, k'cs'l) <- infer'expr left
  (preds'op, t'op, t'cs'op, k'cs'op) <- infer'expr op
  (preds'r, t'r, t'cs'r, k'cs'r) <- infer'expr right

  {-
    The type of the operator should be (at least) a binary function.
    Where the type of the first argument should be the type of the left.
    And the type of the second argument should be the type of the right.
    The type of the result will then be a type of the whole expression.
  -}

  fresh'name <- fresh
  let t'res = T'Var (T'V fresh'name K'Star)

  let t'whole = t'l `type'fn` (t'r `type'fn` t'res)

  let (t'c, k'c) = t'whole `unify'types` t'op

  return  (preds'l ++ preds'op ++ preds'r
          , t'res
          , t'c : t'cs'l ++ t'cs'op ++ t'cs'r
          , k'c : k'cs'l ++ k'cs'op ++ k'cs'r)

-- TODO: check if it's really valid
infer'expr (Tuple exprs) = do
  (preds, types, cs, k'cs) <- foldM infer' ([], [], [], []) exprs
  return (preds, T'Tuple $ reverse types, cs, k'cs)
    where
      infer' (preds, types, constrs, k'constrs) expr = do
        (preds, t, cs, k'cs) <- infer'expr expr
        return (preds, t : types, cs ++ constrs, k'cs ++ k'constrs)

-- TODO: check if it's really valid
infer'expr (If condition then' else') = do
  (preds'cond, t1, t'c1, k'c1) <- infer'expr condition
  (preds'tr, t2, t'c2, k'c2) <- infer'expr then'
  (preds'fl, t3, t'c3, k'c3) <- infer'expr else'
  let (t'c, k'c) = t1 `unify'types` t'Bool
  let (t'b, k'b) = t2 `unify'types` t3
  return  (preds'cond ++ preds'tr ++ preds'fl
          , t2
          , t'c : t'b : t'c1 ++ t'c2 ++ t'c3
          , k'c : k'b : k'c1 ++ k'c2 ++ k'c3)

infer'expr (Let decls body) = do
  -- I will need to do some dependency analysis to split the declarations into groups
  -- and infer the types in the scope of the each group
  -- there's just a small problem
  -- the future implementation of `infer'decls` will probably need to import the infer'expr function
  -- so there's a cycle
  -- I will need to break it somehow or use .hs-boot file
  -- also will there happen the generalization for the declaration?
  -- I think so - each group should generalize the declarations at most at the end of the work for that group
  -- so the next group have the correct generalized types
  (preds'decls, assumptions'decls, t'cs'decls, k'cs'decls) <- infer'decls decls
  (preds'body, t'body, t'cs'body, k'cs'body) <- merge'into't'env assumptions'decls (infer'expr body)
  return (preds'decls ++ preds'body, t'body, t'cs'decls ++ t'cs'body, k'cs'decls ++ k'cs'body)

-- | TODO: I need to read a section about type checking explicitly annotated bindings
-- |        from that I think I should be able to derive the plan for this specific case.
infer'expr (Ann expr qual'type) = do
  undefined
  -- so according the paper this is what should happen:
  {-  Freshly instantiate the implicit type scheme given by the user.
      That most likely means I will need to quantify the qualified type first.
      Destructure the result (Qualified Type) into a list of qualifiers and a type.
  -}
  {-  Infer the type of the expression and unify it with the type-part of the previous step.
      __ I am deriving this process from the part of the paper which infers a type for the annotated
      __ binding group - that means there are Alternatives and there's a function *tiAlts*
      __ which also takes a type as its argument. That type is then unified with the type of
      __ a right hand side - for all alternatives. But I strongly suspect, it doesn't matter if
      __ I pass that type in the *tiAlts* function or not - I can always unify all the RHS types with
      __ it later. So I should be able to do the same here. As it doesn't seem to be used in *tiAlts*
      __ for anything else.
      
      The unification produces a [Constraint Type]. Called Constraints.
      It also produces [Predicate]. Called Predicates.
      Then I should solve the Constraints and obtain the Substitution representing the solution.  -}

  {-  I shall apply the Substitution to the Qualifiers from the first step and get Qualifiers'.
      I shall also apply it to the Type from the first step and get Type'.

      I then apply it to the typing context and find all free type variables in the result. Caled *fs*.
      I then find all free type variables in the Type' and remove all free type variables in *fs*.
        I call the result *gs*.

      I then make the (Qualifiers' :=> Type') into a Type Scheme generalizing over variables in the *gs*.
      Called Scheme'.

      I then apply the Substitution to the Predicates (from the second step),
      then I filter all such Predicates which are entailed by the context Qualifiers'.
      What remains is *split* according the generalized variables.
      And if there are any retained predicates -> error is reported
      signalizing that the declared context is too weak.

      BUT this begs the question: can there be a case like this?
      ...
      ...
      ...    __e :: (P1 a, P2 b) => ...

      And it would be the case that some of the inferred predicates from the __e woule be taken care of
      by the surrounding context? Like (P3 b).

      And I think that this would be illegal. If I am going to give type annotation,
      I must give the full and correct one. If there is a (P3 b) inferred, and I say in my type annotation
      that it is not there - I am saying that this constraint is not a part of the requirements for the
      expression. That means I am denying that requirement. Right?
      And that is pretty much obviously wrong.


      WRONG!
      This piece of code is perfectly valid:

      {-# LANGUAGE ExplicitForAll, ScopedTypeVariables #-}
      class Pred a
      class Aft a

      foo :: forall a b . (Pred a, Aft a) => a -> b -> a
      foo x y
        = bar (x :: a) :: Pred a => a

      bar :: forall a . (Pred a, Aft a) => a -> a
      bar a = a :: a

      That means, that when inferring the type of annotated term, I can in fact leave some constraints
      for surrounding context. That might mean I will need to drop the check for the emptiness of retained
      predicates.

      Yes - I've been thinking about it now (27.1.2020) and I think it goes like this.
      If the Explicitly annotated expression is only "local" then it might be useful to allow this behaviour.
      So in the case of "local annotation/declaration" I will allow retained predicates to be non empty
      
      and I should discarche those predicates which are in the annotation.
      That is already happening. I am filtering all predicates that are entailed by the context given by the programmer.

      I think it might mean, that I don't really need to call `split`. I just return whatever predicates are NOT entailed.
      And the outer scope should take care of it.
      If the top level inference mechanism - concerned with the inference for the whole strongly connected component - for instance
      gets a non-empty list of deffered predicates/constraints. It should raise and error, reporting that the context for the top-level declaration
      is too weak.
      But local type annotations are not forced to state the whole context, they can just state the part of it.
  -}

  {-  Original scheme - as given by the programmer - and the Scheme' are then compared.
      This is the tricky part - as Jones represents Schemes in a different way than I do.
      His representation always has the same order of abstracted type variables and it uses nameless
      convention I think. Because of the first difference, comparing two schemes is very easy and straight forward.
      That is not the case for me and my representation.
      So I will need to inspect Jones'es convention and how the comparison works and translate that into my design.

      In any case - they are supposed to be equal modulo alpha conversion - I think.
      
      Major question: If the Type part of the original scheme (after the substitution) is unified
      with the type inferred by the analysis, could those two be not equal?
      Error reported with not equality is "Signature too general".
      So that probably means, cases like - I say it's gonna be *a* but it is very much an *Int*.
      In this case, the inference would figure out the *Int* part and unifying the *a* with the *Int*
      would not make it an *a* - it would stay being an *Int*.
      Then in the Scheme' there would still be the *Int* and not a type variable *a*.


  -}

infer'expr (Case expr matches) = do
  {- Infer the type of the expr. -}
  (preds'expr, type'expr, t'cs'expr, k'cs'expr) <- infer'expr expr
  
  {- Infer the types of the list of matches. -}
  -- results :: [([Type], Type, [Predicate], [Predicate], [Constraint Type], [Constraint Kind])]
  results <- mapM infer'match matches

  -- each match should produce:
    -- Types
      -- a type - for the expression (RHS)
      -- and a list of types - for the list of patterns
      --      in this case the list of types (for patterns) is going to contain only a single type
    -- Predicates
      -- list of predicates from a list of patterns
      -- list of predicates from an expression
      --  I should be able to just append them together without any problem
    -- Constraints
      -- type constraints from the expression (patterns do not produce type constraints, only assumptions)
      -- kind constraints from the expression
      --    Even though patterns produce assumptions - but the local bindings are used only in the RHSs
      --    and that means, that these assumptions do not escape the context of the Match
  
  -- I then need to unify all the types in the list of types (in this case a singleton) together
  -- that ensures that all patterns are going to match the same thing.
  -- That same thing must be a type of the expr.
  -- So instead - I must take the list of list of types (but in this case it's a list of singletons)
  -- and map that to [Constraint Type] by - for each singleton - unifying that singleton element with a type'expr.
  let (t'cs'patterns, k'cs'patterns) = unzip [ type'expr `unify'types` type'patt | ([type'patt], _, _, _, _, _) <- results ]


  -- Then I need to take a list of types (types of the right hand sides) and map that to the list of
  -- (Constraint Type) by unifying them all with a new fresh variable.
  -- Asserting, that all the right sides are of the same type.
  -- That type is also the result of this whole case expression.
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  {- Now assert that Types of all Right Hand Sides are the same thing. -}
  let (t'cs'rhs's, k'cs'rhs's) = unzip [ t'var `unify'types` type'rhs | (_, type'rhs, _, _, _, _) <- results ]

  -- Now I need to concatenate all the Predicates coming both from Pattern and Right Hand Side.
  {- I think I can mix them together, because from the standpoint of the whole expression - it doesn't
      matter, what part of the expression requires that constraints / produces that Predicate
      it just means, it is needed.
      That also means, that the function infer'match could maybe produce a single [Predicate]. -}
  let preds = preds'expr ++ concat [ preds'patts ++ preds'rhs | (_, _, preds'patts, preds'rhs, _, _) <- results ]

  {-  I also need to concatenate all type constraints and kind constraints from list of matches together
      to those I also need to add constraints from the expr at the top of this branch. -}
  let t'cs = concat $ t'cs'patterns
                    : t'cs'rhs's
                    : t'cs'expr
                    : [ t'cs'patts | (_, _, _, _, t'cs'patts, _) <- results ]
  let k'cs = concat $ k'cs'expr
                    : k'cs'patterns
                    : k'cs'rhs's
                    : [ k'cs'patts | (_, _, _, _, _, k'cs'patts) <- results ]
  
  -- Now I have taken care of all the memebers of each tuple.
  return (preds, t'var, t'cs, k'cs)

-- | TODO: Introductors (how I call them) already have the type annotation in the typing context/
--          So all I need is to retrieve it from there.
-- infer'expr (Intro name exprs) = do
  -- undefined
