{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}

module Compiler.TypeSystem.Type.Infer.Expression where


import Control.Monad ( foldM )
import Control.Monad.Except ( MonadError(throwError) )


import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Expression ( Expression(..) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type(T'Tuple, T'Forall), Rho'Type, Sigma'Type )

import Compiler.TypeSystem.Infer ( Infer, Type'Check )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Type.Constants ( t'Bool, type'fn )

import Compiler.TypeSystem.Type.Infer.Literal ( infer'lit )
import Compiler.TypeSystem.Type.Infer.Pattern ( infer'pat, infer'pattern, check'pattern )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Match ( infer'match )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Declaration ( infer'decls )

import Compiler.TypeSystem.Utils.Infer ( lookup't'env, merge'into't'env, inst'sigma, unify'fun, check'sigma, infer'rho, check'rho, subs'check, skolemise, generalize, quantify, qualify, instantiate )
import Compiler.TypeSystem.Expected ( Expected (Infer, Check) )
import Compiler.TypeSystem.Actual ( Actual (Checked, Inferred) )
import Compiler.TypeSystem.Error ( Error(Unexpected, Typed'Hole) )
import Compiler.Syntax.TFun ( pattern T'Fun )


infer'expr :: Expression -> Expected Rho'Type -> Type'Check ([Predicate], Actual Rho'Type)
infer'expr (Var var'name) expected = do
  sigma <- lookup't'env var'name expected
  inst'sigma sigma expected

infer'expr (Const const'name) expected = do
  sigma <- lookup't'env const'name expected
  inst'sigma sigma expected

infer'expr (Op op'name) expected = do
  sigma <- lookup't'env op'name expected
  inst'sigma sigma expected

infer'expr (Lit lit) expected = do
  infer'lit lit expected

infer'expr (Abs pattern' body) Infer = do
  (preds, arg'type, assumptions) <- infer'pattern pattern'
  (preds', rho) <- merge'into't'env assumptions (infer'rho body)

  return (preds ++ preds', Inferred (arg'type `T'Fun` rho))

infer'expr (Abs pattern' body) (Check rho) = do
  (arg'type, res'type) <- unify'fun rho
  (preds, assumptions) <- check'pattern pattern' arg'type
  preds' <- merge'into't'env assumptions (check'rho body res'type)
  return (preds ++ preds', Checked)

infer'expr (App fun arg) expected = do
  (preds, fun'type) <- infer'rho fun
  (arg'type, res'type) <- unify'fun fun'type
  preds' <- check'sigma arg arg'type
  (preds'', actual'') <- inst'sigma res'type expected
  return (preds ++ preds' ++ preds'', actual'')

infer'expr (Infix'App left op right) expected = do
  infer'expr (App (App op left) right) expected
  {-
    The type of the operator should be (at least) a binary function.
    Where the type of the first argument should be the type of the left.
    And the type of the second argument should be the type of the right.
    The type of the result will then be a type of the whole expression.
  -}

-- TODO: IMPLEMENT
infer'expr (Tuple [expr'a, expr'b]) expected = do
  (preds'a, actual'a) <- infer'expr expr'a expected

  undefined

-- TODO: IMPLEMENT
infer'expr (Tuple exprs) expected = do
  undefined
  -- (preds, types, cs) <- foldM infer' ([], [], []) exprs
  -- return (preds, T'Tuple $ reverse types, cs)
  --   where
  --     infer' (preds, types, constrs) expr = do
  --       (preds, t, cs) <- infer'expr expr
  --       return (preds, t : types, cs ++ constrs)

infer'expr (If condition then' else') Infer = do
  preds <- check'rho condition t'Bool

  (preds'then, rho'then) <- infer'rho then'
  (preds'else, rho'else) <- infer'rho else'

  preds' <- subs'check rho'then rho'else
  preds'' <- subs'check rho'else rho'then
  
  (skolems, context, rho) <- skolemise rho'then
  
  return (preds ++ preds'then ++ preds'else ++ preds' ++ preds'' {- ++ ctxt -} ++ context, Inferred rho {- Inferred oo -})

infer'expr (If condition then' else') (Check rho) = do
  preds'cond <- check'rho condition t'Bool
  preds'then <- check'rho then' rho
  preds'else <- check'rho else' rho

  let preds = concat [preds'cond, preds'then, preds'else]

  return (preds, Checked)

infer'expr (Let decls body) expected = do
  (preds'decls, assumptions'decls) <- infer'decls decls
  (preds'body, t'body) <- merge'into't'env assumptions'decls (infer'expr body expected)
  return (preds'decls ++ preds'body, t'body)

infer'expr (Ann expr sigma) expected = do
  preds <- check'sigma expr sigma
  (preds', actual') <- inst'sigma sigma expected
  return (preds ++ preds', actual')
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

-- TODO: IMPLEMENT
infer'expr (Case expr matches) expected = do
  undefined
  -- {- Infer the type of the expr. -}
  -- (preds'expr, type'expr, t'cs'expr) <- infer'expr expr
  
  -- {- Infer the types of the list of matches. -}
  -- -- results :: [([Type], Type, [Predicate], [Predicate], [Constraint Type], [Constraint Kind])]
  -- results <- mapM infer'match matches

  -- -- each match should produce:
  --   -- Types
  --     -- a type - for the expression (RHS)
  --     -- and a list of types - for the list of patterns
  --     --      in this case the list of types (for patterns) is going to contain only a single type
  --   -- Predicates
  --     -- list of predicates from a list of patterns
  --     -- list of predicates from an expression
  --     --  I should be able to just append them together without any problem
  --   -- Constraints
  --     -- type constraints from the expression (patterns do not produce type constraints, only assumptions)
  --     -- kind constraints from the expression
  --     --    Even though patterns produce assumptions - but the local bindings are used only in the RHSs
  --     --    and that means, that these assumptions do not escape the context of the Match
  
  -- -- I then need to unify all the types in the list of types (in this case a singleton) together
  -- -- that ensures that all patterns are going to match the same thing.
  -- -- That same thing must be a type of the expr.
  -- -- So instead - I must take the list of list of types (but in this case it's a list of singletons)
  -- -- and map that to [Constraint Type] by - for each singleton - unifying that singleton element with a type'expr.
  -- let t'cs'patterns = [ type'expr `Unify` type'patt | ([type'patt], _, _, _, _) <- results ]


  -- -- Then I need to take a list of types (types of the right hand sides) and map that to the list of
  -- -- (Constraint Type) by unifying them all with a new fresh variable.
  -- -- Asserting, that all the right sides are of the same type.
  -- -- That type is also the result of this whole case expression.
  -- fresh'name <- fresh
  -- let t'var = T'Var (T'V fresh'name K'Star)
  -- {- Now assert that Types of all Right Hand Sides are the same thing. -}
  -- let t'cs'rhs's = [ t'var `Unify` type'rhs | (_, type'rhs, _, _, _) <- results ]

  -- -- Now I need to concatenate all the Predicates coming both from Pattern and Right Hand Side.
  -- {- I think I can mix them together, because from the standpoint of the whole expression - it doesn't
  --     matter, what part of the expression requires that constraints / produces that Predicate
  --     it just means, it is needed.
  --     That also means, that the function infer'match could maybe produce a single [Predicate]. -}
  -- let preds = preds'expr ++ concat [ preds'patts ++ preds'rhs | (_, _, preds'patts, preds'rhs, _) <- results ]

  -- {-  I also need to concatenate all type constraints and kind constraints from list of matches together
  --     to those I also need to add constraints from the expr at the top of this branch. -}
  -- let t'cs = concat $ t'cs'patterns
  --                   : t'cs'rhs's
  --                   : t'cs'expr
  --                   : [ t'cs'patts | (_, _, _, _, t'cs'patts) <- results ]
  
  -- -- Now I have taken care of all the memebers of each tuple.
  -- return (preds, t'var, t'cs)

-- | TODO: Introductors (how I call them) already have the type annotation in the typing context/
--          So all I need is to retrieve it from there.
-- infer'expr (Intro name exprs) = do
  -- undefined

infer'expr (Hole name) expected = do
  throwError $ Typed'Hole name expected