{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PatternSynonyms #-}

module Compiler.TypeSystem.Type.Infer.Expression where


import Control.Monad ( foldM )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Foldable ( find )
import Control.Monad.State ( MonadState(put, get) )



import Compiler.Counter ( fresh )

import Compiler.Syntax.Kind ( Kind(K'Star) )
import Compiler.Syntax.Predicate ( Predicate (Is'In) )
import Compiler.Syntax.Qualified ( Qualified((:=>)) )
import Compiler.Syntax.Expression ( Expression(..) )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type(T'Tuple, T'Forall), Rho'Type, Sigma'Type )
import Compiler.Syntax.TFun ( pattern T'Fun )
import Compiler.Syntax.Overloaded ( Overloaded(..) )
import qualified Compiler.Syntax.Placeholder as Placeholder

import Compiler.TypeSystem.Infer ( Infer, Type'Check, add'constraints )
import Compiler.TypeSystem.Constraint ( Constraint(Unify) )
import Compiler.TypeSystem.Type.Constants ( t'Bool, type'fn )

import Compiler.TypeSystem.Type.Infer.Literal ( infer'lit )
import Compiler.TypeSystem.Type.Infer.Pattern ( infer'pat, infer'pattern, check'pattern )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Match ( infer'match, tc'matches )
import {-# SOURCE #-} Compiler.TypeSystem.Type.Infer.Declaration ( infer'decls )

import Compiler.TypeSystem.Utils.Infer ( lookup't'env, merge'into't'env, inst'sigma, unify'fun, check'sigma, infer'rho, check'rho, subs'check, skolemise, lookup'in'overloaded, unify'pair, fresh'meta )
import Compiler.TypeSystem.Expected ( Expected (Infer, Check) )
import Compiler.TypeSystem.Actual ( Actual (Checked, Inferred) )
import Compiler.TypeSystem.Kind.Infer.Annotation ( kind'specify )
import Compiler.TypeSystem.Error ( Error(Unexpected, Typed'Holes) )
import Compiler.TypeSystem.InferenceState (Infer'State(holes))
import Debug.Trace (trace)


infer'expr :: Expression -> Expected Rho'Type -> Type'Check (Expression, [Predicate], Actual Rho'Type)
infer'expr (Var var'name) expected = do
  sigma <- lookup't'env var'name expected
  (preds, actual) <- inst'sigma sigma expected

  let ty = case expected of
            Check t -> t
            Infer -> case actual of
                      Checked -> error "should never happen"
                      Inferred t -> t

  let oo = if var'name == "foo" then trace ("variable = " ++ var'name ++ "  \n|  sigma= " ++ show sigma ++ "\n|  preds = " ++ show preds ++ "\n|  ty= " ++ show ty ++ "\n|  expected= " ++ show expected) var'name else var'name
  -- TODO: HERE - the var might be overloaded constant, or it might be a method,
  -- or it might be one of mutually recrusive definitions I need some way of
  -- knowing whether it's one of those
  m <- lookup'in'overloaded var'name
  expr' <- case m of
            Nothing -> -- OK, just do the normal thing
              return $ Var var'name
            Just Overloaded -> do
              -- application of the overloaded variable to possibly many
              -- placeholders so I need to know how many dictionaries the
              -- overloaded constant expects where do I get this information?
              -- from the sigma! it will be in the shape: forall [x...] .
              -- (context) => type and I need to know what the context is
              -- depending on how many things are in the context, that many
              -- placeholder arguments

              -- trik je, ze ja potrebuju vedet, do ceho se to jako instanciuje
              -- no nejlepsi by asi tim padem bylo z inst'sigma vratit jeste
              -- jeden mapping mapping z tech rigid typovejch promennejch do
              -- tech instanciovanejch ja uz vracim `preds`, to jsou
              -- instanciovany predikaty, a jsou urcite z te sigmy, pokud budou
              -- urcite v obou modech ve stejnym poradi, tak bych mohl pouzit
              -- proste to a tohle by melo snad platit, v obou modech inst'sigma
              -- vede na volani instantiate, coz by snad melo vest na stejny
              -- poradi v zasade je podle me klicovy akorat to, aby instantiate
              -- a inst'sigma a dalsi funkce dodrzely stejny poradi, jako je ten
              -- typ samotnej ulozenej v kontextu
              let placeholders  = map (\ (Is'In cl'name ty) -> Placeholder $ Placeholder.Dictionary cl'name ty) preds
                  application   = foldl App (Var var'name) placeholders
              return application
            Just (Method cl'name) -> do -- put the placeholder
            -- I think that if it's a method placeholder the type should be
            -- something different when I have something like
            --
            -- foo :: Foo a => a -> a what I actually need is the `a` part, aka
            -- - the type which is qualified by the Foo predicate (assuming foo
            -- is from class Foo) because when I am eliminating this method,
            -- having the a -> a type won't help me much it's not like it is
            -- completely useless, but I will actually need to do the same
            -- "investigative work" only with worse clues so now I need to find
            -- a predicate, which shares the name with the class name of this
            -- method
              case find (\ (Is'In c'n _) -> c'n == cl'name) preds of
                Nothing -> do
                  throwError $ Unexpected ("Could not find a predicate with class'es name '" ++ cl'name ++ "' in the type of '" ++ var'name ++ "'")
                Just (Is'In c'name ty) ->
                  return $ Placeholder $ Placeholder.Method var'name ty cl'name
            Just Recursive -> -- put the placeholder
              return $ Placeholder $ Placeholder.Recursive var'name ty

  return (expr', preds, actual)

infer'expr c@(Const const'name) expected = do
  sigma <- lookup't'env const'name expected
  (preds, actual) <- inst'sigma sigma expected
  return (c, preds, actual)

infer'expr o@(Op op'name) expected = do
  sigma <- lookup't'env op'name expected
  (preds, actual) <- inst'sigma sigma expected

  let ty = case expected of
            Check t -> t
            Infer -> case actual of
                      Checked -> error "should never happen"
                      Inferred t -> t

  -- TODO: HERE - the var might be overloaded constant, or it might be a method,
  -- or it might be one of mutually recrusive definitions I need some way of
  -- knowing whether it's one of those
  m <- lookup'in'overloaded op'name
  expr' <- case m of
            Nothing -> -- OK, just do the normal thing
              return $ Op op'name
            Just Overloaded -> do
              -- application of the overloaded variable to possibly many placeholders
              -- so I need to know how many dictionaries the overloaded constant expects
              -- where do I get this information?
              -- from the sigma!
              -- it will be in the shape: forall [x...] . (context) => type
              -- and I need to know what the context is
              -- depending on how many things are in the context, that many placeholder arguments

              -- trik je, ze ja potrebuju vedet, do ceho se to jako instanciuje
              -- no nejlepsi by asi tim padem bylo z inst'sigma vratit jeste
              -- jeden mapping mapping z tech rigid typovejch promennejch do
              -- tech instanciovanejch ja uz vracim `preds`, to jsou
              -- instanciovany predikaty, a jsou urcite z te sigmy, pokud budou
              -- urcite v obou modech ve stejnym poradi, tak bych mohl pouzit
              -- proste to a tohle by melo snad platit, v obou modech inst'sigma
              -- vede na volani instantiate, coz by snad melo vest na stejny
              -- poradi v zasade je podle me klicovy akorat to, aby instantiate
              -- a inst'sigma a dalsi funkce dodrzely stejny poradi, jako je ten
              -- typ samotnej ulozenej v kontextu
              let placeholders  = map (\ (Is'In cl'name ty) -> Placeholder $ Placeholder.Dictionary cl'name ty) preds
                  application   = foldl App (Op op'name) placeholders
              return application
            Just (Method cl'name) -> do -- put the placeholder
            -- I think that if it's a method placeholder the type should be something different
            -- when I have something like     foo :: Foo a => a -> a
            -- what I actually need is the `a` part, aka - the type which is
            -- qualified by the Foo predicate (assuming foo is from class Foo)
            -- because when I am eliminating this method, having the    a -> a
            -- type won't help me much it's not like it is completely useless,
            -- but I will actually need to do the same "investigative work" only
            -- with worse clues so now I need to find a predicate, which shares
            -- the name with the class name of this method
              case find (\ (Is'In c'n _) -> c'n == cl'name) preds of
                Nothing -> do
                  throwError $ Unexpected ("Could not find a predicate with class'es name '" ++ cl'name ++ "' in the type of '" ++ op'name ++ "'")
                Just (Is'In c'name ty) ->
                  return $ Placeholder $ Placeholder.Method op'name ty cl'name
            Just Recursive -> -- put the placeholder
              return $ Placeholder $ Placeholder.Recursive op'name ty


  return (expr', preds, actual)

infer'expr l@(Lit lit) expected = do
  (preds, actual) <- infer'lit lit expected
  return (l, preds, actual)

infer'expr (Abs pattern' body) Infer = do
  (pattern'', preds, arg'type, assumptions) <- infer'pattern pattern'
  (body', preds', rho) <- merge'into't'env assumptions (infer'rho body)

  return (Abs pattern'' body', preds ++ preds', Inferred (arg'type `T'Fun` rho))

infer'expr (Abs pattern' body) (Check rho) = do
  (arg'type, res'type) <- unify'fun rho
  (pattern'', preds, assumptions) <- check'pattern pattern' arg'type
  (body', preds') <- merge'into't'env assumptions (check'rho body res'type)
  return (Abs pattern'' body', preds ++ preds', Checked)

infer'expr (App fun arg) expected = do
  (fun', preds, fun'type) <- infer'rho fun
  (arg'type, res'type) <- unify'fun fun'type
  (arg', preds') <- check'sigma arg arg'type
  (preds'', actual'') <- inst'sigma res'type expected
  return (App fun' arg', preds ++ preds' ++ preds'', actual'')

infer'expr (Infix'App left op right) expected = do
  infer'expr (App (App op left) right) expected
  {-
    The type of the operator should be (at least) a binary function.
    Where the type of the first argument should be the type of the left.
    And the type of the second argument should be the type of the right.
    The type of the result will then be a type of the whole expression.
  -}

-- TODO: IMPLEMENT
infer'expr (Tuple [expr'a, expr'b]) Infer = do
  -- TODO: just infer those two types and put them in the tupple type I guess
  (expr'a', preds'a, type'a) <- infer'rho expr'a
  (expr'b', preds'b, type'b) <- infer'rho expr'b
  return (Tuple [expr'a', expr'b'], preds'a ++ preds'b, Inferred $ T'Tuple [type'a, type'b])

infer'expr (Tuple [expr'a, expr'b]) (Check t) = do
  -- TODO: how do we do that?
  -- I need to write a function, which will unify the expected type with a two-ple type
  -- the two-ple type will contain two type variables right? similar to the function which unifies with function type
  -- 
  (type'a, type'b) <- unify'pair t
  (expr'a', preds'a) <- check'rho expr'a type'a
  (expr'b', preds'b) <- check'rho expr'b type'b

  return (Tuple [expr'a', expr'b'], preds'a ++ preds'b, Checked)

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
  (condition', preds) <- check'rho condition t'Bool

  (then'', preds'then, rho'then) <- infer'rho then'
  (else'', preds'else, rho'else) <- infer'rho else'

  preds' <- subs'check rho'then rho'else
  preds'' <- subs'check rho'else rho'then
  
  (skolems, context, rho) <- skolemise rho'then
  
  return (If condition' then'' else'', preds ++ preds'then ++ preds'else ++ preds' ++ preds'' ++ context, Inferred rho)

infer'expr (If condition then' else') (Check rho) = do
  (cond', preds'cond) <- check'rho condition t'Bool
  (then', preds'then) <- check'rho then' rho
  (else', preds'else) <- check'rho else' rho

  let preds = concat [preds'cond, preds'then, preds'else]

  return (If cond' then' else', preds, Checked)

infer'expr (Let decls body) expected = do
  (decls', preds'decls, assumptions'decls) <- infer'decls decls
  (body', preds'body, t'body) <- merge'into't'env assumptions'decls (infer'expr body expected)
  return (Let decls' body', preds'decls ++ preds'body, t'body)

infer'expr (Ann expr sigma) expected = do
  -- TODO: fully specify kinds within types in the `sigma`
  sigma' <- kind'specify sigma
  (expr', preds) <- check'sigma expr sigma'
  (preds', actual') <- inst'sigma sigma' expected
  return (Ann expr' sigma, preds ++ preds', actual')
  -- so according the paper this is what should happen:
  {-  Freshly instantiate the implicit type scheme given by the user. That most
      likely means I will need to quantify the qualified type first. Destructure
      the result (Qualified Type) into a list of qualifiers and a type.
  -}
  {-  Infer the type of the expression and unify it with the type-part of the
      previous step. __ I am deriving this process from the part of the paper
      which infers a type for the annotated __ binding group - that means there
      are Alternatives and there's a function *tiAlts* __ which also takes a
      type as its argument. That type is then unified with the type of __ a
      right hand side - for all alternatives. But I strongly suspect, it doesn't
      matter if __ I pass that type in the *tiAlts* function or not - I can
      always unify all the RHS types with __ it later. So I should be able to do
      the same here. As it doesn't seem to be used in *tiAlts* __ for anything
      else.
      
      The unification produces a [Constraint Type]. Called Constraints. It also
      produces [Predicate]. Called Predicates. Then I should solve the
      Constraints and obtain the Substitution representing the solution.  -}

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

infer'expr (Case expr matches) expected = do
  {-
    if I use tc'matches - I can rely on the fact that each match only contains a single patter
      therefore the function type given by that function will be
      <pattern type> -> <result type>

      <pattern type> should always be the same as <expr type> and
      <result type> should be <expected> if <expected is not Infer>

      so I can take advantage of that
  -}

  -- first infer the type of a motive
  (motive, preds'motive, type'motive) <- infer'rho expr
  -- if I am in Check-ing mode
  -- I should propagate the type into tc'matches
  -- to do that, though, I need to construct the correct function type:
  -- from the motive'type to expected type
  -- if I am in infer mode
  -- I stay in infer mode

  let match'type
        = case expected of
            Infer -> Infer
            Check ty -> Check $ type'motive `T'Fun` ty

  -- now I can run the tc'matches
  (matches', preds'matches, actual'match) <- tc'matches matches match'type

  -- now if the `actual` is (Inferred t) I need to deconstruct that type and just take the result part
  -- I can do that using existing function unify'fun

  actual'result <- case actual'match of
                    Checked -> return Checked
                    Inferred ty -> do
                      (in't, out't) <- unify'fun ty
                      -- now I need to unify the in'type with the type of the motive
                      -- because in Infer mode, nothing would connect those two types
                      add'constraints [in't `Unify` type'motive]
                      return $ Inferred out't

  -- now I just return everything I have collected
  return (Case motive matches, preds'motive ++ preds'matches, actual'result)

infer'expr (Hole name) Infer = do
  meta'var <- fresh'meta
  state <- get
  let holes' = (name, meta'var) : holes state
  put $ state{ holes = holes' }
  return (Hole name, [], Inferred meta'var)

infer'expr (Hole name) (Check t) = do
  state <- get
  let holes' = (name, t) : holes state
  put $ state{ holes = holes' }
  return (Hole name, [], Checked)


infer'expr (Placeholder _) expected = do
  throwError $ Unexpected "Infering type for placeholder - should have never happened!"
