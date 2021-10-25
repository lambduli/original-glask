{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Syntax.ToAST where


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

  to'ast (Term'E'App t'exprs) = undefined
  -- TODO: implement! This will use my implementation of the Extended Shunting Yard algorithm.

  to'ast (Term'E'Tuple t'exprs) = do
    exprs <- mapM to'ast t'exprs
    return $ Tuple exprs 

  to'ast (Term'E'List exprs)
    = to'ast $ Term'E'App $ intersperse (Term'E'Op $ Term'Id'Const ":") $ exprs ++ [Term'E'Id $ Term'Id'Const "[]"]

  to'ast (Term'E'Arith'Seq t'begin may'step t'end)
    = undefined
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


instance To'AST Term'Pat Pattern where
  to'ast (Term'P'Id (Term'Id'Var var'name)) = do
    return $ P'Var var'name

  to'ast (Term'P'Id (Term'Id'Const const'name)) = do
    undefined

  to'ast (Term'P'Op (Term'Id'Var var'name)) = do
    return $ P'Var var'name

  to'ast (Term'P'Op (Term'Id'Const var'name)) = do
    undefined

  to'ast (Term'P'Lit literal) = do
    return $ P'Lit literal

  to'ast (Term'P'App t'pats) = do
    {-  Here is the interesting part, I first need to use the Extended Shunting Yard Algorithm to correctly parenthesize/split the whole Pattern Application.
        Then I might get Pattern Sub-Expressions like: Cons ... ... ...
        where Cons stands for a Term'P'Id (Term'Id'Const _)
        What I will need to do is go over all the newly constructed Term'P'App and if the first member in the list is a Cons (from above)
        I will need to change the whole Term'P'App into a P'Con

        IMPORTANT: I think I should do that only after running the ESYA first.
        So that means I will do either of those:
          1)  I will run the ESYA and get the result
              But because the result is still the Term'Pat I will need to translate it into a Pattern and for that I may need to call some other function,
              because the to'ast would loop forever on the Term'P'App

          2)  I can integrate the translation of the Cons ... pattern into a specific implementation of ESYA for Patterns

        BUT:  If I decide that I will first run the ESYA on the Term'Expression input and that way I will correctly parenthesise it.
              Then translate it from the Expression to Pattern, then I think I may not need to concern myself with these details.
              Instead of this instance I will implement instance To'AST Expression Pattern and for Application Expression I will do the important thing.
              I LIKE THIS IDEA BETTER I THINK.
    -}

    undefined

  to'ast (Term'P'Labeled name t'fields) = do
    {-  This will be translated into a (P'Con Name [Pattern]).
        For such desugar I need to have a Constructor Analysis information ready.

        THOUGHT:  If I decide to translate to Expression and from that to Pattern, this will already be taken care of.
        So I would only need to take care of the Cons Pattern case.
    -}

    undefined

  to'ast (Term'P'Tuple t'pats) = do
    pats <- to'ast t'pats
    return $ P'Con (tuple'name'for $ length t'pats) pats
    --             ^^^ or something like that
      where tuple'name'for num = "(" ++ replicate num ',' ++ ")"

  to'ast (Term'P'List t'pats) = do
    -- TODO: use P'Con Pattern constructor, create sequence like a : b : ... : z : []
    undefined

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


instance To'AST Term'Type Type where
  to'ast (Term'T'Id (Term'Id'Var var)) = undefined

  to'ast (Term'T'Id (Term'Id'Const con)) = undefined

  to'ast (Term'T'Tuple t'types) = undefined

  to'ast (Term'T'List t'type) = undefined
  
  {- This relies on the fact that (->) is registered in the fixity environment
      and the shunting yard algorithm is implemented for types too.
      
    In case I change my mind about that -> just do it the simple way. -}
  to'ast (Term'T'Arrow t'types) = do
    to'ast $ Term'T'App $ intersperse (Term'T'Id (Term'Id'Const "(->)")) t'types

  to'ast (Term'T'App t'types) = undefined
  -- TODO: implement shunting yard algorithm


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
  --          if yes --> I need to pick the left-most operation/function
  --            if the Infix'App makes it to the final implementation, this would be the place where it would be used
  --            I would take the middle part and somehow deconstruct the rest of the pattern parameters
  --            because what I store in the Binding'Group is a list of Matches
  --            that means I need the complete list of Patterns
  --            For that exact reason, it would perhaps be better to start with correctly parenthesizing the t'pat with ESYA
  --            and then manually transforming it into a [Match] and Binding'Group eventually.
  --            Skipping the step where I translate it using a to'ast directly (calling it at the top level or something like that).

  to'ast (Term.Signature name t'qual'type) = do
    qual'type <- to'ast t'qual'type
    return $ AST.Signature $ AST.T'Signature name qual'type

  to'ast (Term.Data'Decl name params t'constr'decls) = do
    constr'decls <- to'ast t'constr'decls
    return $ AST.Data'Decl name params constr'decls

  to'ast (Term.Type'Alias name params t'type) = do
    type' <- to'ast t'type
    return $ AST.Type'Alias name params type'

  to'ast (Term.Fixity fixity level name) = do
    return $ AST.Fixity fixity level name

  to'ast (Term.Class cl'name var'name t'preds t'decls) = do
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
