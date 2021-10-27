{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Syntax.ToAST where


import qualified Data.Set as Set
import Data.List (intersperse, replicate, find)
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map


import Compiler.Syntax.Term
import qualified Compiler.Syntax.Term as Term
import Compiler.Syntax.Term.Expression

import Compiler.Syntax
import qualified Compiler.Syntax as AST
import Compiler.Syntax.Expression
import qualified Compiler.Syntax.ToAST.TranslateEnv as Trans'Env
import Compiler.Syntax.ToAST.Translate
import Compiler.Syntax.ToAST.SemanticError
import Compiler.Analysis.Syntactic.ConstrEnv
import qualified Compiler.Analysis.Syntactic.ConstrEnv as CE
import Compiler.Syntax.ToAST.ESYA
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE
import Compiler.Syntax.ToAST.Utils.Translate
import Compiler.Analysis.TypeSystem.Type.Constants
import Compiler.Analysis.TypeSystem.Solver.Substitutable


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

  to'ast (Term'E'Ann t'expr (t'preds, t'type)) = do
    expr <- to'ast t'expr
    preds <- to'ast t'preds
    type' <- to'ast t'type
    return $ Ann expr (preds :=> type')

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
    constr'env <- asks Trans'Env.constructors
    case constr'env Map.!? name of
      Nothing -> throwError $ Not'In'Scope'Data name
      Just (Constr _) -> do
        if null field'assigns -- Constr {}
        then return $ Const name -- Constr
        else throwError $ Wrong'Fields name $ map fst field'assigns -- error
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

  -- NOTE: think about qualifying the field names, maybe I could make it so that field names are qualified
  -- with the type prefix --> so there can be multiple fields in the same module (of the same name)
  -- and maybe with the help of type classes I can figure out how to have the same field names in the single data type
  -- like each constructor has a field named `foo` (or not necessarily each)
  -- that seems like a simple class desurgaring

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


instance To'AST a b => To'AST [a] [b] where
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
    typing'context <- asks TE.typing'scope
    -- NOTE: even though this type variable should always be in the context
    --        I might make a mistake in the implementation -> better be safe.
    case typing'context Map.!? var of
      Nothing -> error "Unexpected behaviour: While assigning a Kind to a type variable, I have approached a type variable which is not in the typing context."
      Just kind -> return $ T'Var $ T'V var kind

  to'ast (Term'T'Id (Term'Id'Const con)) =  do
    typing'context <- asks TE.typing'scope
    -- NOTE: even though this type constant should always be in the context
    --        I might make a mistake in the implementation -> better be safe.
    case typing'context Map.!? con of
      Nothing -> error "Unexpected behaviour: While assigning a Kind to a type constant, I have approached a type constant which is not in the typing context."
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
  to'ast (Term.Binding t'pat t'expr) = undefined
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
    kind'context <- asks TE.typing'scope
    -- now I keep only those type variables which are not scoped and are therefore seen for the first time
    let only'actually'free = filter (`Map.member` kind'context) free'variables
    -- those need to be assigned a new and fresh Kind Variable
    new'kind'context <- Map.fromList <$> mapM (\ name -> return (name, K'Var <$> fresh)) only'actually'free

    -- now, this new kind context needs to be merged into a current invironment and
    -- with use of `local` the translation of the whole Qualified Type Term shall proceed
    -- TODO: continue with that and correct following lines

    qual'type <- to'ast t'qual'type
    return $ AST.Signature $ AST.T'Signature name qual'type

  to'ast (Term.Data'Decl name params t'constr'decls) = do
    -- TODO: Here I also need to register all the type variables
    constr'decls <- to'ast t'constr'decls
    return $ AST.Data'Decl name params constr'decls

  to'ast (Term.Type'Alias name params t'type) = do
    -- TODO: Here I also need to register all the type variables
    type' <- to'ast t'type
    return $ AST.Type'Alias name params type'

  to'ast (Term.Fixity fixity level name) = do
    return $ AST.Fixity fixity level name

  to'ast (Term.Class cl'name var'name t'preds t'decls) = do
    -- TODO: Here I also need to register all the type variables
    -- since it is a class declaration, there's going to be just the one `var'name`
    preds <- to'ast t'preds
    decls <- to'ast t'decls
    return $ AST.Class cl'name var'name preds decls

  to'ast (Term.Instance t'qual'pred t'decls) = do
    qual'pred <- to'ast t'qual'pred
    decls <- to'ast t'decls
    return $ AST.Instance qual'pred decls


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
