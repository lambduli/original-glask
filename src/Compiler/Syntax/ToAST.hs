{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Compiler.Syntax.ToAST where


import Data.List (intersperse)


import Compiler.Syntax.Term
import Compiler.Syntax.Term.Expression

import Compiler.Syntax
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
  -- TODO: implement

  to'ast (Term'E'Tuple t'exprs) = do
    exprs <- mapM to'ast t'exprs
    return $ Tuple exprs 

  to'ast (Term'E'List exprs)
    = to'ast $ Term'E'App $ intersperse (Term'E'Op $ Term'Id'Const ":") $ exprs ++ [Term'E'Id $ Term'Id'Const "[]"]

  to'ast (Term'E'Arith'Seq t'begin may'step t'end)
    = undefined
    -- TODO: implement later - this is going to be rewritten with class methods

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

  to'ast (Term'E'Labeled'Constr name field'assigns)
    = undefined
  -- TODO: I first need to do constructor analysis
  -- collect all the important information about constructors from their declarations
  -- like the fields fixed in the right order -- their types
  -- so that record syntax than can be desugared into ordinary data types
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
  to'ast = undefined


instance To'AST a b => To'AST [a] [b] where
  to'ast as = mapM to'ast as


instance To'AST (Term'Pat, Term'Expr) Match where
  to'ast (t'pat, t'expr) = do
    pattern' <- to'ast t'pat
    rhs <- to'ast t'expr
    return $ Match { patterns = [pattern'], rhs = rhs }


-- TODO: maybe remove this, depends whether I will use it in the to'ast for Term'E'Labeled'*
instance To'AST a b => To'AST (Name, a) (Name, b) where
  to'ast (name, a) = do
    b <- to'ast a
    return (name, b)


instance To'AST Term'Type Type where
  to'ast = undefined


instance To'AST Term'Pred Predicate where
  to'ast = undefined


instance To'AST Term'Decl Declaration where
  to'ast = undefined
