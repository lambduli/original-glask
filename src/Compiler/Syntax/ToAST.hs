{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Syntax.ToAST where


import Data.List (intersperse, replicate)


import Compiler.Syntax.Term
import qualified Compiler.Syntax.Term as Term
import Compiler.Syntax.Term.Expression

import Compiler.Syntax
import qualified Compiler.Syntax as AST
import Compiler.Syntax.Expression

import Compiler.Syntax.ToAST.Translate


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

  to'ast (Term'E'Labeled'Constr name field'assigns) = do
    undefined
  -- TODO: I first need to do constructor analysis
  -- collect all the important information about constructors from their declarations
  -- like the fields fixed in the right order -- their types
  -- so that record syntax then can be desugared into ordinary data types
  -- the constructor analysis is also important for when I need to generate some aditional code
  -- like in case of costructor identifier - registering a lambda with a Intro construct inside
  -- also register the getter for each field name

  -- NOTE: think about qualifying the field names, maybe I could make it so that field names are qualified
  -- with the type prefix --> so there can be multiple fields in the same module (of the same name)
  -- and maybe with the help of type classes I can figure out how to have the same field names in the single data type
  -- like each constructor has a field named `foo`
  -- that seems like a simple class desurgaring

  to'ast (Term'E'Labeled'Update t'expr field'assigns) = undefined
  -- TODO: implement
  -- same as above, some previous constructor analysis is going to be necessary


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
