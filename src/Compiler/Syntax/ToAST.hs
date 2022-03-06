{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Syntax.ToAST where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.List as List

import Data.Sequence ( unzipWith )
import Data.List (intersperse, replicate, find)
import Data.Maybe ( mapMaybe )
import Control.Monad.Trans.Reader ( asks, local )
import Control.Monad.Except ( when, replicateM, MonadError(throwError) )
import Control.Monad.State ( when, replicateM )


import Compiler.Counter ( fresh )

import qualified Compiler.Syntax.Term as Term
import Compiler.Syntax.Term.Declaration ( Term'Constr'Decl, Term'Decl )
import Compiler.Syntax.Term.Identifier ( Term'Id(..) )
import Compiler.Syntax.Term.Pattern ( Term'Pat(..) )
import Compiler.Syntax.Term.Predicate ( Term'Pred )
import Compiler.Syntax.Term.Expression ( Term'Expr(..) )
import Compiler.Syntax.Term.Type ( Term'Type(..) )

import qualified Compiler.Syntax as AST
import Compiler.Syntax.BindGroup ( Bind'Group(..) )
import Compiler.Syntax.Declaration ( Constr'Decl, Data(Data), Declaration(..) )
import Compiler.Syntax.Kind ( Kind(K'Var) )
import Compiler.Syntax.Match ( Match(..) )
import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Pattern ( Pattern(..) )
import Compiler.Syntax.Predicate ( Predicate )
import Compiler.Syntax.Qualified ( Qualified(..) )
import Compiler.Syntax.Type ( T'C(T'C), T'V(..), Type(..) )
import Compiler.Syntax.Expression ( Expression(..) )

import Compiler.Syntax.ToAST.ESYA ( ESYA(process) )
import Compiler.Syntax.ToAST.Translate ( run'translate, Translate )
import Compiler.Syntax.ToAST.TranslateState ( Translate'State )
import qualified Compiler.Syntax.ToAST.TranslateEnv as Trans'Env

import Compiler.TypeSystem.Type.Constants ( type'fn, type'list )
import Compiler.TypeSystem.Solver.Substitutable ( Term(free'vars) )
import Compiler.TypeSystem.Utils.Infer ( close'over )

import Compiler.Analysis.Syntactic.ConstrEnv ( Constr'Info(Record, Constr, fields) )
import qualified Compiler.Analysis.Syntactic.ConstrEnv as CE

import Compiler.Analysis.Semantic.SemanticError ( Semantic'Error(..) )


-- NOTE and TODO:
-- The second argument counter ::Int is just a temporary solution to the problem I have ran into.
-- When I want to build a Trans'Env I need to collect all user defined type constructors and assign them a Kind build from fresh Kind Variables
-- BUT, to do that, I need to be inside a State monad. Because I need to be able to increment the counter.
-- On a first glance, it would seem that I could be inside a Translate monad stack, BUT that is not the case, since I am, at that moment, building
-- that very thing (collecting all the parts of the Trans'Env)
-- So I am left with the other choice - I say I am in the context of the State monad which only contains the Counter
-- that means, however, that I will need to pick the final value of the counter AFTER I collect all the type constructors and assign them a Kind
-- and use that as a intial value for the counter in the Translate'State
-- for that exact reason, this parameter needs to be passed through few levels and be used here

-- TODO: this function also needs to merge all the binding groups of the same name together

translate :: To'AST a b => a -> Translate'State -> Trans'Env.Translate'Env -> Either Semantic'Error (b, Translate'State)
translate a trans'state trans'env
    = run'translate trans'env (to'ast a) trans'state -- (to'ast a :: Translate a)

-- translate'to'ast :: [Term'Decl] -> Int -> TE.Translate'Env -> Either Semantic'Error [Declaration]
-- translate'to'ast declarations counter trans'env
--   = -- TODO: now to run the translation using some sort of run'X function
--   run'translate counter trans'env (to'ast declarations :: Translate [Declaration])


-- NOTE:  this function should be somewhere else
--        I still think, that having to translate from the AST to the Program, Binding'Group and so on, immediately after doing so much work to get the AST is sort of awkward
--        but then again, the AST is not entirely lost, I just replace [Declaration] with Program ([[Explicit], [[Implicit]]] or something like that) which is reasonable
--        the type inference doesn't need to concern itself with other declarations
--        BUT then again - maybe it would be reasonable to merge the process of collecting kind constraints and type constraints on the top level point of view
--        Then I would need to include type declarations into the collection given to the "constraint finding process"
--        so that would maybe mean something like: I give some function the whole [Declaration] collection
--        and IT will split it into a Program and the rest (for type declarations) [I won't need fixity declarations at this point]
--        that would somehow solve my issue with exposing the detail of sorting and transforming to Program on this TOP LEVEL

-- TODO:  I need to collect bindings inside type class declarations and instance declarations too
--        First of -> my current parser doesn't allow type classes to contain a default implementations of methods
--        For the instance bindings ->
--          I should collect type annotations from the type classes
--          I should collect methods (which are strictly untyped)
--          If I combine those into a Map, it would need to be a Map Name (Qualified Type, [Bind'Group])
--            because I have potentially many Bind'Groups per each name/qualified type
--            each name MUST have a qualified type, but it could potentially have no implementations/Bind'Groups
--          The thing is - I don't really need a Map, I don't think I would ever do an explicit lookup, so maybe a Set or a List would be enough
--          But anyway - now I should have all the methods from the instances explicitly typed, so I should just transform it into a "special kind of explicits"
--          special because there is many Bind'Groups per a single type annotations
--          Maybe I could merge all the Bind'Groups into a single Bind'Group to utilize the existing infrastructure for the type analysis
--            I don't think I can do that!
--      !!  I need to 



class To'AST a b where
  to'ast :: a -> Translate b


instance To'AST Term'Expr Expression where
  to'ast (Term'E'Id (Term'Id'Var name))
    = return $ Var name
  
  to'ast (Term'E'Id (Term'Id'Const name))
    = return $ Const name

  to'ast (Term'E'Op (Term'Id'Var op))
    = return $ Op op

  to'ast (Term'E'Op (Term'Id'Const op))
    = return $ Op op

  to'ast (Term'E'Lit literal)
    = return $ Lit literal

  to'ast (Term'E'Abst t'pat t'body) = do
    pattern' <- to'ast t'pat
    body <- to'ast t'body
    return $ Abs pattern' body

  -- TODO: for now, I am assuming, that each application will be parenthesized
  to'ast (Term'E'App [left, right]) = do
    left'ast <- to'ast left
    right'ast <- to'ast right
    return $ App left'ast right'ast

  to'ast (Term'E'App t'exprs) =
    error "Not implemented: Expression Term Application with mutliple arguments --> AST"
  -- TODO: implement! This will use my implementation of the Extended Shunting Yard algorithm.

  to'ast (Term'E'Tuple t'exprs) = do
    exprs <- mapM to'ast t'exprs
    return $ Tuple exprs 

  to'ast (Term'E'List exprs)
    = to'ast $ Term'E'App $ intersperse (Term'E'Op $ Term'Id'Const ":") $ exprs ++ [Term'E'Id $ Term'Id'Const "[]"]

  to'ast (Term'E'Arith'Seq t'begin may'step t'end)
    = error "Not implemented: Arithmetic sequences --> AST"
    -- TODO: implement later - this is going to be rewritten/desugared with class methods from Enum or something like that.

  to'ast (Term'E'If t'condition t'then t'else) = do
    condition <- to'ast t'condition
    then' <- to'ast t'then
    else' <- to'ast t'else
    return $ If condition then' else'

  to'ast (Term'E'Let t'decls t'expr) = do
    decls <- to'ast t'decls
    expr <- to'ast t'expr
    return $ Let decls expr

  -- TODO: refactor - higher-rank
  to'ast (Term'E'Ann t'expr (t'preds, t'type)) = do
    let 
        free'in'preds :: Set.Set Term'Id
        free'in'preds = free'vars t'preds
        
        free'in'type :: Set.Set Term'Id
        free'in'type  = free'vars t'type
        
        gen'vars :: [Term'Id]
        gen'vars = Set.toList $ free'in'preds `Set.union` free'in'type
        
        names :: [Name]
        names = map (\ (Term'Id'Var n) -> n) gen'vars
    
    fresh'names <- mapM (const fresh) names

    let kinds = map K'Var fresh'names

    let assumptions :: [(Name, Kind)]
        assumptions = zip names kinds
        t'vs = zipWith T'V names kinds

    expr  <- merge'into'k'env assumptions (to'ast t'expr)
    preds <- merge'into'k'env assumptions (to'ast t'preds)
    type' <- merge'into'k'env assumptions (to'ast t'type)

    when (not $ Set.null $ free'in'type `Set.difference` free'in'preds) (throwError $ Ambiguous'Type preds type')
    
    -- if not $ Set.null $ free'in'type `Set.difference` free'in'preds
    --   then throwError $ Umbiguous'Type preds type'
    --   else ()


    -- NOTE:  I generalize/quantify over such variables which are free in the qualified type
    --        ScopeTypeVar  -- this doesn't implement scoped type vars -- so if I want them later - I need to refactor this
    let sigma = T'Forall t'vs (preds :=> type')

    return $ Ann expr sigma

  to'ast (Term'E'Case t'expr t'alts) = do
    expr <- to'ast t'expr
    alts <- to'ast t'alts
    return $ Case expr alts

  {-  Labeled Construction looks like this:
      Constr'Name{ field'name_1 = value_1 , ... , field'name_n = value_n }
      the `name` argument is the Constr'Name and the field'assigns is the list of pairs of field assignments
      what I need to create from that is:
      Constr'Name value_1 ... value_n
      where the values must be in the correct order - the order is given by the order in the Constr Analysis -}

  --
  --  TODO: I have added a new data structure into the Translate Environment.
  --  It is called `fields` and it should allow me to quickly find whether some field name is part of some constructor and which.
  --  That should make some operations in the following code bit more straightforward.
  --  So - REFACTOR PLS.
  --
  to'ast (Term'E'Labeled'Constr name field'assigns) = do
    -- first check that the constructor `name` is actually declared
    constr'env <- asks Trans'Env.constructors
    case constr'env Map.!? name of
      Nothing ->
        throwError $ Not'In'Scope'Data name

      -- so the constructor name belongs to the positional constructor
      Just (Constr _) -> do
        if null field'assigns -- Constr {} <-- this is awkward but generally allowed
        then return $ Const name -- Constr <-- I will alow it
        else throwError $ Wrong'Fields name $ map fst field'assigns -- error <-- trying to invoke a non-record data-constructor as a record one

      -- this is the right shape
      Just Record{ fields = fields } -> do
        {-  I need to go over field'assigns and check that each of them is actual field of the current constructor. -}
        {-  That is - to check that the code is not trying to use any field which is not technically a field of the constructor. -}
        let wrong'fields = filter (not . flip elem fields) $ map fst field'assigns
        {-  All the field names from `field'assigns` which are NOT in the `fields` --> those are the `wrong'fields` -}
        
        {-  I need to check that all the declared fields are initialized. -}
        {-  In Haskell failing to initialize all fields will raise a Warning, I am going to be more restrictive and raise an Error. -}
        let 
            only'field'names = map fst field'assigns
            uninitialized'fields = filter (not . flip elem only'field'names) fields
        {-  all the fields that are not in the `field'assigns` --> those are the uninitialized'fields -}

        if not . null $ wrong'fields
        then throwError $ Wrong'Fields name wrong'fields
        else if not $ null uninitialized'fields
          then throwError $ Uninitialized'Fields name uninitialized'fields
          else  do
                -- just do the desugaring finally
                -- I shoudl just map the fields to the corresponding values
                let t'values = mapMaybe (`lookup` field'assigns) fields
                values <- to'ast t'values :: Translate [Expression]
                -- that puts the values in the correct order
                -- then I can just construct the Application
                let app = foldl App (Const name) values
                return app

  to'ast (Term'E'Labeled'Update t'expr field'assigns) = do
  {-  First I need to identify which specific Constructor all the fields mentioned in the field'assigns belong to.
      I am going to do it like this:
        - I will find a constructor which has a field named as the FIRST field in the field'assigns sequence
          - if NO constructor is found --> raise an arror saying that "the field name" is not in scope
          - if exactly one constructor is found --> check that it is a Record and all the "updated fields" are present in the Record info
            - if NOT --> raise an error saying that there's no constructor defining all the fields
            - if YES --> proceed with the desugaring
  -}
    constr'info <- look'for'constr field'assigns t'expr
    case constr'info of
      Constr _ -> error "Unexpected behaviour: function look'for'constr returned `Constr _` instead of Record{}"
      Record{ CE.name = name, fields = fields } -> do
        -- now I can actually desugar the expression
        -- we begin with something like: expr{ field_1 = val_1, ..., field_n = val_n }
        -- we now know what Constructor is going to be used
        -- so what I need to do is - I think - create a case expression which deconstructs the current expression
        -- and pattern matches on all the values with just variables
        -- then uses the variables to fill the not-mentioned properties
        -- and replace those mentioned with the values given
        -- I should make some function like patch or something to figure it out
        expr <- to'ast t'expr
        let letters = [1 ..] >>= flip replicateM ['a' .. 'z']
        let var'names = map (letters !!) [0 .. length fields]
        let pattern = P'Con name $ map P'Var var'names  -- to budou vsechno promenny o poctu `length fields`

        -- NOTE: I map the sequence of the (Variable'From'Pattern, Field'Name)
        --        to a sequence of Expression
        --        every time I find a position which corresponds to the field name which is in the field'assigns I pick the assigned value instead
        let var'names'on'field'names = zip var'names fields
        let var'or'update :: (Name, Name) -> Translate Expression
            var'or'update (var'name, field'name)
              = case lookup field'name field'assigns of
                Nothing -> return $ Var var'name
                Just term'expr -> do
                  to'ast term'expr

        exprs <- mapM var'or'update var'names'on'field'names
        -- NOTE: now I fold it all together, constructing the RHS of the pattern
        let rhs = foldl App (Const name) exprs
        let the'match = Match { patterns = [pattern], rhs = rhs }
        return $ Case expr [the'match]


look'for'constr :: [(Name, Term'Expr)] -> Term'Expr -> Translate Constr'Info
look'for'constr field'assigns t'expr = do
  if null field'assigns
  then throwError $ Empty'Record'Update t'expr
  else do
    -- get the name of any field from the non-empty sequence of pairs
    -- I choose to pick the first one
    let ((field'name, _) : assigns) = field'assigns
    -- now I need to find the Record Constr'Info which contains this field
    constr'env <- asks Trans'Env.constructors
    field'env <- asks Trans'Env.fields

    let field'names :: [Name]
        field'names = map fst field'assigns

    case field'env Map.!? field'name of
      Nothing ->
        throwError $ Not'In'Scope'Field field'name
      Just (Constr _) ->
        error "Unexpected behaviour: the lookup for the constructor with a specific field returned an ordinary constructor without fields."
      Just constr@Record{ fields = fields } -> do
        -- I can now iterate over all the field names in the `field'assigns`
        -- I can check whether each field name IS or IS NOT defined
        -- I can first focus on the field names which are not defined
        -- if there are NO UNDEFINED field names some of them still can be 'from some other constructor'
          -- I will need to check that all the field names in the `field'assigns` are in fact the fields from this current constructor
        
        case find (not . flip Map.member field'env) field'names of
          Just undefined'field'name ->
            throwError $ Not'In'Scope'Field undefined'field'name
          Nothing -> do
            -- now check that all the field names in the `field'names` are present in this constructor (in the `fields` sequence)
            case filter (not . flip elem fields) field'names of
              (_ : _) -> throwError $ No'Constructor'Has'All'Fields field'names
              [] -> return constr



{- TODO: What needs to be done - I need to correctly parenthesize the pattern applications.
          For that I will use my ESYA.
        Then I need to translate Term'P'App into a P'Con.
          If I understand correctly - in case of patterns - the only acceptable Pattern Application
          will be an application of the constructor identifier to one or many patterns.
          Knowing that - I can always assume that Pattern Applications have to start with
          Pattern Identifier (Term'P'Id (Term'Id'Const _)) and then there can be any Pattern.
          So every time I see a Term'P'App, I can be sure that after correctly parenthesizing it
          with ESYA, its first element must be a constructor identifier.

          And because it is not allowed to make patterns like: `((Cons head) tail)`
          I don't need to worry about collapsing the Pattern Applications.
                In this case it would be possible to collapse correctly.
                But what if the constructor is an operator: `(head :) tail`
                  this might seem like almost understandable, but in Haskell, this doesn't work.
                  Also - with my postfix operators, it may be possible to make it work just fine.
                  But realize that the `:` is probably POST-fix constructor (binary) in this example
                  (otherwise it wouldn't make sense and would not parse)
                  and `head :` means it is partially applied, so it still waits for another argument
                  then the whole expression is applied to another argument - all good right?
                  Except that the POST-fix constructor operator `:` somehow finds itself
                  in the middle, in a very strange position.
                  It should look like this: `head tail :`. Puting the POST-fix `:` anywhere else
                  than at the end is just unreasonable.

        So - when translating Term'P'App I first need to use ESYA to create the correct structure
          then I should always obtain a Term'P'App (because I don't have an Infix App Pattern)
          that means I will get the list of Term'Patterns - being arguments to the first Pattern
          in the Term'P'App list. That list of Term'Patterns (without the first Constructor)
          needs to be translated to'ast too. It can be done by `mapM` and no problem at all should arise.

        If I implement the ESYA as a type class. And one of the methods will be something like
          on'value'accept which should mean, that the Term'Pat value is being pushed to the output
          and before that, I have an option to do something with it.
          I could call `to'ast` on that value. That would mean, that the resulting output sequence
          would contain Patterns and not Term'Pats. That would also mean, that I would as a result of the ESYA
          get a value of the Pattern.
          To make it work, means ESYA needs to call some externally-defined function, which at the end,
          transforms the sesquence of the Patterns into a P'Con pattern
          something like `on'whole'sequence`. And that would make it possible to inspect the whole sequence
          and in case of Patterns -> check that the first thing in the application is always a constructor
          and then change it to P'Con.

          I kinda like that idea. It makes the general idea of ESYA customizable to the great extent.

          Change of plans - that wouldn't work, because the types.

          Instead I will make the ESYA produce a sequence of Term'Pat in the postfix.
          Then when translating the sequence into a valid Pattern tree, I will do all the
          checking and translation of the Term'P'App into a P'Con
-}
instance To'AST Term'Pat Pattern where
  to'ast (Term'P'Id (Term'Id'Var var'name)) =
    return $ P'Var var'name

  to'ast (Term'P'Id (Term'Id'Const const'name)) =
    return $ P'Con const'name []

  to'ast (Term'P'Op (Term'Id'Var var'name)) =
    return $ P'Var var'name

  to'ast (Term'P'Op (Term'Id'Const const'name)) =
    return $ P'Con const'name []

  to'ast (Term'P'Lit literal) =
    return $ P'Lit literal

  -- NOTE: This is just momentary implementation for simple patterns
  to'ast (Term'P'App (constr't'pat : arg't'pats)) = do
    arg'pats <- mapM to'ast arg't'pats
    case constr't'pat of
      Term'P'Id (Term'Id'Const con'name) -> -- desugar into P'Con
        return $ P'Con con'name arg'pats
      Term'P'Op (Term'Id'Const con'op'name) -> -- desugar into P'Con
        return $ P'Con con'op'name arg'pats
      _ -> error "Not implemented: Pattern Application Term --> AST"

  to'ast (Term'P'App t'pats) = do
    let in'postfix :: [Term'Pat]
        in'postfix = process t'pats
    -- TODO: now I need to translate the sequence in postfix of Term'Pat
    --      into a single Pattern value
    --      along the way I will use the information from the analysis about operators
    --      their fixities, associativity, and precedence (sometimes also arity? or does that mean, that POST and PRE will only be unary?)
    --      and for each actual value I will need to call to'ast to produce Pattern
    --      it also means, that the last thing I need to do, is take the constructor
    --      which should be the FIRST (in POSTfix) element in this list
    --      and together with the rest of the list
    --      make it a P'Con
    --      then I can return this value, as will all the calls on the smaller parts
    --      which will produce a valid Pattern value and it will type check
    error "Not implemented: Pattern Application Term --> AST"

  {-  Example: Constr'Name{ foo = <pattern> } -}
  --  TODO: Implement after you refactor the record stuff in Expression
  to'ast (Term'P'Labeled name t'fields) = do
    {-  This will be translated into a (P'Con Name [Pattern]).
        For such desugar I need to have a Constructor Analysis information ready.
    -}
    error "Not implemented: Labeled pattern --> AST"

  to'ast (Term'P'Tuple t'pats) = do
    pats <- to'ast t'pats
    return $ P'Con (tuple'name'for $ length t'pats) pats
    --             ^^^ or something like that
      where tuple'name'for num = "(" ++ replicate num ',' ++ ")"

  to'ast (Term'P'List t'pats) = do
    -- TODO: use P'Con Pattern constructor, create sequence like a : b : ... : z : []
    error "Not implemented: List Patter Term --> AST"

  to'ast (Term'P'As name t'pat) = do
    pat <- to'ast t'pat
    return $ P'As name pat

  to'ast Term'P'Wild = do
    return P'Wild


{-  NOTE: This instance implements the merging of the Bind'Groups together.
          When there are two or more Bindings with the same name - they should get merged into a single Binding.
          This instance does exactly that.
          It expects these invariants:
            - The declarations are written in such a way so that all Bindings from the same group are always together.
            - It doesn't matter that it changes the order of Declarations in limited way. Specifically - it should be OK
              to put all the Bindings at the end (preserving their order) behind all the other Declarations (preserving their order too).
-}
instance {-# OVERLAPPING #-} To'AST [Term'Decl] [Declaration] where
  to'ast as = to'ast'
    where
      declarations :: Translate [Declaration]
      declarations = mapM to'ast as

      to'ast' :: Translate [Declaration]
      to'ast' = do
        decls <- declarations
        let (b'g'decls, other'decls)  = List.partition is'b'group decls
            b'groups                  = map to'b'g b'g'decls
            grouped                   = List.groupBy same'group b'groups
            merged                    = map merge'b'group grouped

        return $ other'decls ++ merged

      is'b'group :: Declaration -> Bool
      is'b'group (Binding _)  = True
      is'b'group _            = False

      to'b'g :: Declaration -> Bind'Group
      to'b'g (Binding bind'group) = bind'group -- NOTE: Although partial - it should never fail

      same'group :: Bind'Group -> Bind'Group -> Bool
      same'group Bind'Group{ name = name'a } Bind'Group{ name = name'b }
        = name'a == name'b

      merge'b'group :: [Bind'Group] -> Declaration
      merge'b'group b'groups =
        {-  NOTE: There should always be just one match per Bind'Group, but just to be sure, I am going to handle it so that it doesn't metter. -}
        let all'matches = concatMap (\ Bind'Group{ alternatives = matches } -> matches) b'groups
            the'name    = name . head $ b'groups -- NOTE: Although it is partial it should never fail - nor the `head` nor the names should differ
        in  Binding $ Bind'Group{ name = the'name, alternatives = all'matches }


instance {-# OVERLAPPABLE #-} To'AST a b => To'AST [a] [b] where
  to'ast as = mapM to'ast as


instance To'AST (Term'Pat, Term'Expr) Match where
  to'ast (t'pat, t'expr) = do
    pattern' <- to'ast t'pat
    rhs <- to'ast t'expr
    return $ Match { patterns = [pattern'], rhs = rhs }


-- TODO: maybe remove this, depends whether I will use it in the to'ast for Term'E'Labeled'*
-- I also use it to translate Con'Record'Decl
instance To'AST a b => To'AST (Name, a) (Name, b) where
  to'ast (name, a) = do
    b <- to'ast a
    return (name, b)


{- NOTE: So I think that if I make a requirement/assumption that:
          before `to'ast` is called on the type
          first - information about all free type variables is retrieved
          only type variables which are not in the current environment are considered free
          and those are going to be registered in the environment
          but first to each one of those there's going to be a fresh Kind Variable
          assigned.
          Because of that - later when actually translating using `to'ast`
          when a type variable/constant is approached -> it's kind variable
          will be looked up in the environment, so that each occurence of that type variable
          will share the same Kind Variable.

        The same typing context should also contain the kind associations for all type constants.
        Like type constructors - primitives and user-defined.
   -}
instance To'AST Term'Type Type where
  to'ast (Term'T'Id (Term'Id'Var var)) = do
    kinding'context <- asks Trans'Env.kind'context
    -- NOTE: even though this type variable should always be in the context
    --        I might make a mistake in the implementation -> better be safe.
    
    case kinding'context Map.!? var of
      Nothing -> throwError $ Internal $ "Unexpected behaviour: While assigning a Kind to a type variable, I have approached a type variable `" ++ show var ++ "` which is not in the kind context."
      Just kind -> return $ T'Var $ T'V var kind

  to'ast (Term'T'Id (Term'Id'Const con)) =  do
    kinding'context <- asks Trans'Env.kind'context
    -- NOTE: even though this type constant should always be in the context
    --        I might make a mistake in the implementation -> better be safe.
    case kinding'context Map.!? con of
      Nothing -> throwError $ Internal $ "Unexpected behaviour: While assigning a Kind to a type constant, I have approached a type constant `" ++ show con ++ "` which is not in the kind context."
      Just kind -> return $ T'Con $ T'C con kind

  to'ast (Term'T'Tuple t'types) = do
    types <- mapM to'ast t'types
    return $ T'Tuple types

  to'ast (Term'T'List t'type) = do
    type' <- to'ast t'type
    -- NOTE: now I relly on the fact, that list type constructor is going to be defined in the prelude
    return $ T'App type'list type'
  
  {- This relies on the fact that (->) is registered in the fixity environment
      and the shunting yard algorithm is implemented for types too.
      
    In case I change my mind about that -> just do it the simple way. -}
  to'ast (Term'T'Arrow t'types) = do
    types <- mapM to'ast t'types
    -- [a, b, c] ~~> (a -> b -> c)
    -- (a -> (b -> c))
    -- (-> a ((-> b) c))
    return $ foldr1 type'fn types -- I can use the line below any time later
    -- to'ast $ Term'T'App $ intersperse (Term'T'Id (Term'Id'Const "(->)")) t'types

  -- TODO: for now I assume that I don't have type operators
  --        that makes the type applications pretty straightforward
  --        (a b c d) ~~> (((a b) c) d)
  -- TODO: later implement the extended shunting yard algorithm
  --        which will allow me to support various type operators
  -- NOTE: assuming that `t'types` are not empty (though they should always contain more than 1 element)
  to'ast (Term'T'App t'types) = do
    types <- mapM to'ast t'types
    return $ foldl1 T'App types

  {-  DESCRIPTION:  Explicit foralls with no quantified variables are implicitly unwrapped. -}
  --                so types like `forall . Maybe Int` and `Maybe Int` are the same thing
  --                The unwrapping is OK, because
  --                    - if this type is inside a bigger type - it's no harm to strip the redundant forall
  --                    - if this type is THE type in the type annotation - it's OK - to'ast for Signatures closes over all the types
  to'ast (Term'T'Forall [] ([], t'type)) = do
    to'ast t'type

  to'ast (Term'T'Forall vars t'qual'type) = do
    fresh'names <- mapM (const fresh) vars
    
    let names = map (\ (Term'Id'Var n) -> n) vars
        kinds = map K'Var fresh'names
        assumptions = zip names kinds
        tv's = zipWith T'V names kinds
    
    qual'type <- merge'into'k'env assumptions (to'ast t'qual'type)
    
    return $ T'Forall tv's qual'type


instance To'AST Term'Pred Predicate where
  to'ast (Term.Is'In name t'type) = do
    type' <- to'ast t'type
    return $ AST.Is'In name type'


-- NOTE: Data declarations and Type Synonyms will introduce new local kind variables for new local type variables
instance To'AST Term'Decl Declaration where
  {- TODO: implement ... this one is going to be tricky -}
  -- I am going to need to produce Bind'Group - at this stage it will just contain a single match
  
  -- Match contains list of Patterns and rhs :: Expression
  -- t'expr is going to be translated directly to be the rhs
  -- t'pat must first be translated and then reconstructed - the top level node of the pattern will become a name of the Bind'Group
  -- the rest of it will become a part of the singleton [Match]

  -- THERE IS A NOTE IN THE FORM OF A QUESTION IN THE MODULE FOR MATCH
  -- PLEASE CONSULT IT FIRST, THINK ABOUT IT AND FIND A SATISFIABLE ANSWER
  to'ast (Term.Binding t'pat t'expr) = do
    expr <- to'ast t'expr

    case t'pat of
      Term'P'Op _ -> do
        pat <- to'ast t'pat :: Translate Pattern
        error "Not Implemented: Operator binding."

      -- TODO:  I am going to implement just a simple variable binding for now.
      --        That means that there are no arguments -> no patterns.
      --        I will need to implement the rest later.
      Term'P'Id (Term'Id'Var var'name) -> do
        return $ AST.Binding $ Bind'Group{ AST.name = var'name, alternatives = [ Match{ patterns = [], rhs = expr } ] }


      Term'P'App t'pats -> do
        -- TODO: now I use ESYA and translate the list of patterns and
        error "Not Implemented: to'ast for Term Binding for non-variable (parametrized) bindings"

      _ -> error "Not Implemented: This is a Semantic Error - bad binding, it should never happen if my parser is correct, but also, it should be either made impossible by the type system or covered by some constructor of Semantic'Error (TODO: get on it)"
      -- LATEST NOTE: I am handling it this way (differently for variable binding and function binding)
      -- because I haven't yet decided how am I going to implement ESYA for Patterns
      -- because normally Patterns would never really look like:
      -- `foo [pattern] [pattern] ... [pattern]`
      -- where `foo` is some NON-CONSTRUCTOR identifier, aka just a variable
      -- that would be an error in normal Patterns
      -- but here I would very much need to allow such behaviour
      -- and what's more - that case is actually one of only two valid forms I recognise
      -- so either my ESYA for Patterns is going to somehow know about this
      -- or I will need to only use ESYA for Patterns on the data, which won't make it break


  -- TODO: I will need to translate the t'pat (Term'Pattern) into a Pattern
  --        That should give me something like Pattern Application on the top level
  --          if not --> raise an error (syntactic) = this binding's pattern is not syntactically correct
  --          if yes --> I need to pick the left-most (top-most? [but not necessarily at the complete root]) operation/function
  --            if the Infix'App makes it to the final implementation, this would be the place where it would be used
  --            I would take the middle part and somehow deconstruct the rest of the pattern parameters
  --            because what I store in the Binding'Group is a list of Matches
  --            that means I need the complete list of Patterns
  --            For that exact reason, it would perhaps be better to start with correctly parenthesizing the t'pat with ESYA
  --            and then manually transforming it into a [Match] and Binding'Group eventually.
  --            Skipping the step where I translate it using a to'ast directly (calling it at the top level or something like that).

  -- to'ast (Term.Signature name (context, Term'T'Forall t'ids t'qual'type)) = do
  --   {-  TODO: check that `context` is empty. It must be, because if the type itself is EXPLICIT forall,
  --             then there must not be any context.

  --       TODO: It must also be checked that the `tvs` contains all free type variables in the t'qual'type.
  --             Either there's going to be forall quantifying over all free type variables - or there must not be the explicit forall.
  --   -}
  --   -- now I need to assign each variable in the `tvs` fresh kind variable
  --   fresh'names <- mapM (const fresh) t'ids
    
  --   let kinds       = map K'Var fresh'names
  --       names :: [Name]
  --       names       = map (\ (Term'Id'Var n) -> n) t'ids
  --       assignments = zip names kinds
  --       tvs :: [T'V]
  --       tvs         = map (uncurry T'V) assignments

  --   qual'type <- merge'into'k'env assignments (to'ast t'qual'type)
  --   return $ AST.Signature $ AST.T'Signature name $ T'Forall tvs qual'type

  {-  DESCRIPTION:  This case handles signatures with types like:
                    foo :: forall a . a -> Maybe a
                    baz :: forall a b . a -> b -> a
                    bar :: forall . Maybe Int
  -}
  to'ast (Term.Signature name ([], fr@(Term'T'Forall _ _))) = do
    {-  TODO: check that `context` is empty. It must be, because if the type itself is EXPLICIT forall,
              then there must not be any context. -}
    
    type' <- to'ast fr

    {-  DESCRIPTION:  Because to'ast can strip the fr from the outermost forall - I need to check what shape is the resulting type' in. -}
    {-  If it's T'Forall --> all free variables in both context and type must be quantified.  -}
    {-  If it's just any other Type constructor --> I can assume that the outermost forall was redundant (no context and no free variables) and convert it to Sigma.  -}
    let sigma = case type' of
                  T'Forall _ _ -> type'
                  ty -> close'over $ [] :=> ty

    return $ AST.Signature $ AST.T'Signature name sigma

  {-  DESCRIPTION:  This case handles signatures like:
                    foo :: Show a => forall b . a -> b -> String
                    bar :: Show a => a -> String
      and so on.                                                   -}
  to'ast (Term.Signature name t'qual'type) = do
    -- TODO: here is the place where I need to find all the free type variables
    --        keep only those, which are not "scoped" (already in the kind context)
    --        and register them with fresh Kind Varaible

    -- destructure the term qualified type
    let (t'preds, t'type) = t'qual'type

    -- first to get the Set of Term'Ids (but only Term'Id'Var actually)
    let free'from'type = free'vars t'type :: Set.Set Term'Id

    -- now also the type variables from the type context part
    let free'from'context = foldl (\ set' t' -> Set.union set' (free'vars t')) Set.empty t'preds :: Set.Set Term'Id
    
    -- now combine them together
    -- NOTE: following lambda is partial, but since free'variables only contains variables and not constants, that should be OK
    -- TODO: perhaps revisit this piece of code and maybe decide to return Set of Strings instead to fix this weak spot
    let free'variables = map (\ (Term'Id'Var name) -> name) $ Set.toList $ free'from'context `Set.union` free'from'type

    -- now remove all those variables which are already in the kind context
    -- so first I need to get the kind context
    kind'context <- asks Trans'Env.kind'context
    -- now I keep only those type variables which are not scoped and are therefore seen for the first time
    let only'actually'free = filter (not . (`Map.member` kind'context)) free'variables
    -- those need to be assigned a new and fresh Kind Variable
    fresh'names <- mapM (const fresh) only'actually'free -- fresh name for each one of actually free type variables
    let kinds = map K'Var fresh'names -- fresh kind variable for every fresh name
    let assignments = zip only'actually'free kinds -- put them together to create a list of kind assignments

    -- now, those new kind assigmemnts needs to be merged into a current invironment and

    context <- merge'into'k'env assignments (to'ast t'preds)
    type'   <- merge'into'k'env assignments (to'ast t'type)

    let sigma = case type' of
                      (T'Forall tvs (ctxt :=> ty)) ->
                        -- the forall type must be collapsed - context must get merged
                        close'over $ (context `List.union` ctxt) :=> ty
                      ty ->
                        -- just qualify it with the context and close over it
                        close'over $ context :=> ty



    -- qual'type@(context :=> type') <- merge'into'k'env assignments (to'ast t'qual'type) -- Show a => forall b . a -> b -> a
    return $ AST.Signature $ AST.T'Signature name sigma -- $ close'over qual'type

  to'ast (Term.Data'Decl name params t'constr'decls) = do
    -- NOTE: Start by getting the Kind of this Type Constructor
    k'ctxt <- asks Trans'Env.kind'context
    -- NOTE: even though this type variable should always be in the context
    --        I might make a mistake in the implementation -> better be safe.
    --        Or if the user writes :k a -> a   ==> then a will not be recognized as known type variable
    k <- case k'ctxt Map.!? name of
      Nothing ->
        throwError $ Internal "Unexpected behaviour: While translating a Data declaration I have approached a type constructor which is not in the kind context."
      Just kind ->
        return kind

    {-  `params` are type variable names which need to be assigned a fresh kind variable each -}
    fresh'names <- mapM (const fresh) params
    let kinds = map K'Var fresh'names
    let assignments = zip params kinds

    constr'decls <- merge'into'k'env assignments (to'ast t'constr'decls)

    let k'params = map (uncurry T'V) assignments

    return $ AST.Data'Decl $ Data (T'C name k) k'params constr'decls

  to'ast (Term.Type'Alias name params t'type) = do
    {-  `params` are type variable names which need to be assigned a fresh kind variable each -}
    fresh'names <- mapM (const fresh) params
    let kinds = map K'Var fresh'names
    let assignments = zip params kinds

    type' <- merge'into'k'env assignments (to'ast t'type)

    return $ AST.Type'Alias name params type'

  to'ast (Term.Fixity fixity level name) = do
    return $ AST.Fixity fixity level name

  to'ast (Term.Class'Decl cl'name var'name t'preds t'decls) = do
    {-  NOTE: My current implementation doesn't allow nested/scoped classes
              That means, that I don't need to worry about scoped type variables.
              Simply - the class' type variable is not going to be scoped.
              Later I could introduce scoped/nested class declarations
              then I would need to revisit this place and fix it.
     -}

    -- TODO:  I need to translate each type signature one by one
    --        each time computing set of free variables only within that type annotation + the type class parameter - that is shared amongst all of them

    -- let's start with the trivial substitution  var'name -> <fresh kind>

    -- I actually already have the Kind of the class' parameter in the kind'context
    cl'env <- asks Trans'Env.classes
    
    param'kind <- case cl'env Map.!? cl'name of
      Nothing -> throwError $ Internal "Unexpected: I have come across a type class which is not registered in the kind context."
      Just kind -> return kind


    -- fr'name <- fresh
    -- let param'kind = K'Var fr'name
    let triv'assmpt :: (Name, Kind)
        triv'assmpt = (var'name, param'kind)

    -- now I need to one-by-one map the Signatures in the Class - each time registering all free type variables within it
    let method'decl'to'ast :: Term.Term'Decl -> Translate Declaration
        method'decl'to'ast sign@(Term.Signature _ (t'ctxt, t'type)) = do
          -- getting all the free type variables, except the type-class-parameter - that one already has a Kind
          let free'loc'ids = Set.delete (Term'Id'Var var'name)  (free'vars t'ctxt `Set.union` free'vars t'type)
              free'loc'vars = map (\ (Term'Id'Var name) -> name) $ Set.toList free'loc'ids

          -- each free type variable needs to be assigned a new and fresh Kind Variable
          fresh'names <- mapM (const fresh) free'loc'vars
          let kinds = map K'Var fresh'names
              assumptions = zip free'loc'vars kinds

          merge'into'k'env (triv'assmpt : assumptions) (to'ast sign)

        method'decl'to'ast _ = undefined -- TODO: Currently I don't allow default definitions in Type Classes. This case should not happen then.

    decls <- mapM method'decl'to'ast t'decls

    -- predicates/super classes of the current class must only refer to the `type parameter` (var'name) - so this is enough
    preds <- put'in'k'env triv'assmpt (to'ast t'preds)

    return $ AST.Class'Decl (AST.Class cl'name (T'V var'name param'kind) preds decls)

  to'ast (Term.Instance t'qual'pred t'decls) = do
    {-  NOTE: My current implementation doesn't allow nested/scoped instances
              That means, that I don't need to worry about scoped type variables.
              Simply - the instance's type variable is not going to be scoped.
              Later I could introduce scoped/nested instance declarations
              then I would need to revisit this place and fix it.
    -}
     {- NOTE: The `t'qual'pred` thing is a pair of [Term'Pred] and Term'Pred
              The list represents context and the second value is THE instance.
              Regarding type variables - there can be type variables in both of those.
              Importantly, I think I always need to use all the free variables from the list part
              inside the second part. (But I think I don't need to worry about it here. It's going to be checked in the process of typechecking.)
              What it means for me, I find all the free variables in both of those - using union.
        EXAMPLES:
          instance Foo (Either a b)
          instance (Show a, Show b, Num a, Num b) => Foo (Either a b)
     -}
    let (t'preds, t'pred) = t'qual'pred
    let variables = Set.toList $ free'vars t'preds `Set.union` free'vars t'pred
    let names = map (\ (Term'Id'Var name) -> name) variables
    -- NOTE: the lambda is not total, but `free'vars` should never give Term'Id'Const _

    fresh'names <- mapM (const fresh) variables
    let kinds = map K'Var fresh'names
    let assignments = zip names kinds

    -- NOTE: I don't know if I need to register the variable to translate predicates, but it shouldn't hurt
    qual'preds <- merge'into'k'env assignments (to'ast t'qual'pred)
    decls <- merge'into'k'env assignments (to'ast t'decls)
    return $ AST.Instance qual'preds decls


-- TODO: this function is just duplicate code of (multiple) helper functions in the Infer.hs in Utils module
-- do something about it
-- I could write them as general as possible (and useful) and put them all in some shared Utils module
put'in'k'env :: (String, Kind) -> Translate a -> Translate a
put'in'k'env (var'name, kind) m = do
  let scope e@Trans'Env.Trans'Env{ Trans'Env.kind'context = k'ctx } = e{ Trans'Env.kind'context = Map.insert var'name kind $ Map.delete var'name k'ctx }
  local scope m

-- TODO: same as the function above
merge'into'k'env :: [(String, Kind)] -> Translate a -> Translate a
merge'into'k'env bindings m = do
  let scope e@Trans'Env.Trans'Env{ Trans'Env.kind'context = k'ctx } = e{ Trans'Env.kind'context = Map.fromList bindings `Map.union` k'ctx}
  local scope m


instance (To'AST a b) => To'AST ([Term'Pred], a) (Qualified b) where
  to'ast (t'preds, a) = do
    preds <- to'ast t'preds
    b <- to'ast a
    return $ preds :=> b


instance To'AST Term'Constr'Decl Constr'Decl where
  to'ast (Term.Con'Decl name t'types) = do
    types <- to'ast t'types
    return $ AST.Con'Decl name types

  to'ast (Term.Con'Record'Decl name t'field'types) = do
    field'types <- to'ast t'field'types
    return $ AST.Con'Record'Decl name field'types
