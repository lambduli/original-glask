module Compiler.Analysis.Semantic.Synonym.FullyApplied where

import Control.Monad.Reader
import Control.Applicative (Applicative(liftA2))
import Control.Monad.Extra
import qualified Data.Map.Strict as Map

import Compiler.Syntax.Term
import Compiler.Syntax.Term.Type

import Compiler.Analysis.Syntactic.SynonymEnv

import Compiler.Analysis.Semantic.SemanticError
import Compiler.Syntax.Term.Expression

type Partially'Applied'Synonyms = Reader Synonym'Env [Semantic'Error]

class Find'Error a where
  find :: a -> Partially'Applied'Synonyms

-- | TODO: check for Synonym cycles

-- | TODO:  consider collecting type constraints from the program containing synonyms too
--          maybe it could give some nicer error messages?
--          just think about it and write the result somewhere


-- | TODO: expand synonyms (not here, but somewhere)

-- | TODO: I think I need to check that all type synonyms are fully applied here
-- | I don't know why it wasn't mentioned here before, but that seems to be the most important thing when doing Synonym Analysis.
-- | Of course together with checking for synonym cycles.

{-  Fully Applied Analysis - if there's a type synonym which is not fully applied.
    I think it would be much simpler to analyze this thing on the Term'Type, because in the Term'Type - the Term'T'App has a list of Term'Type.
    That means that checking the number of parameters would be much simpler.

-}


-- | This function walks the whole list of declarations as parsed (Term'Decl).
-- | It's goal is to find some type constructor, which is a type synonym, which is not fully applied.
-- | So each Term'T'App needs to be checked like this:
-- |    Check the first element in the Term'T'App's argument --> if it's a Term'T'Id I need to check if it's Term'Id'Const
-- |    if it is --> check whether it is a known Type Synonym (in the Synonym'Env) --> if yes, check that it is applied to as many arguments as the number in the Synonym'Env declares.
-- |    if it isn't --> return an Error (Synonym'Not'Partially'Applied'Synonyms)
check :: Synonym'Env -> [Term'Decl] -> [Semantic'Error]
check syn'env term'decls
  = concatMap (\ decl -> runReader (find decl) syn'env) term'decls


instance Find'Error Term'Decl where
  find (Binding term'pattern term'expr)
    = find term'expr
    
  find (Signature name (qualifiers, term'type))
  --  aliases for Constraints are not allowed -- therefore it's not necessary to check term'preds
    = find term'type
    
  find (Data'Decl type'name type'params term'constr'declarations)
    = concat <$> mapM find term'constr'declarations
    
  find (Type'Alias alias'name type'params term'type)
    = find term'type
    
  find Fixity{}
    = return []
    
  find (Class class'name type'param supers term'declarations)
    = concat <$> mapM find term'declarations
    
  find (Instance _ term'declarations)
    = concat <$> mapM find term'declarations


instance Find'Error Term'Expr where
  find (Term'E'Id _)
    = return []

  find (Term'E'Op _)
    = return []

  find (Term'E'Lit _)
    = return []

  find (Term'E'Abst _ term'expr)
    = find term'expr

  find (Term'E'App term'exprs)
    = concat <$> mapM find term'exprs

  find (Term'E'Tuple term'exprs)
    = concat <$> mapM find term'exprs

  find (Term'E'List term'exprs)
    = concat <$> mapM find term'exprs

  find (Term'E'Arith'Seq term'expr'a maybe'term'expr'b term'expr'c) = do
    -- a :: [Maybe Error]
    a <- find term'expr'a

    -- b :: [Maybe Error]
    b <- find term'expr'c

    -- c :: [Maybe Error]
    c <- maybeM (pure []) find $ pure maybe'term'expr'b
    return $ a ++ b ++ c

  find (Term'E'If t'e'cond t'e'then t'e'else)
    = concat <$> mapM find [t'e'cond, t'e'then, t'e'else]

  find (Term'E'Let term'decls term'expr)
    = let d = concat <$> mapM find term'decls
          e = find term'expr
      in liftM2 (++) d e
  {- NOTE: I am not sure this way of writing it is the most elegant. I like that it does not feature a `do` though. -}

  find (Term'E'Ann term'expr (term'preds, term'type))
  --  aliases for Constraints are not allowed -- therefore it's not necessary to check term'preds
    = let e = find term'expr
          t = find term'type
      in liftM2 (++) e t
  {- NOTE: The same thing as above. -}

  find (Term'E'Case term'expr term'alts)
    = let e = find term'expr
          as = concat <$> mapM (\ (_, term'expr) -> find term'expr) term'alts
      in liftM2 (++) e as
  {- NOTE: The same thing as above. -}

  find (Term'E'Labeled'Constr _ field'assigns)
    = concat <$> mapM (\ (_, term'expr) -> find term'expr) field'assigns

  find (Term'E'Labeled'Update term'expr field'updates) -- [(Name, Term'Expr)]
    = let e = find term'expr
          us = concat <$> mapM (\ (_, term'expr) -> find term'expr) field'updates
      in liftM2 (++) e us


instance Find'Error Term'Type where
  find (Term'T'Id (Term'Id'Var name))
    = return []

  find (Term'T'Id (Term'Id'Const name))
    = return []

  find (Term'T'Tuple term'types)
    = concat <$> mapM find term'types

  find (Term'T'List term'type)
    = find term'type

  find (Term'T'Arrow term'types)
    = concat <$> mapM find term'types

  {- NOTE:  This one is the critical part. If the first of the types is a `Term'T'Id (Term'Id'Const name)`
            --> I need to check whether that name is a known Type Synonym.
            If yes --> Check it's arity and number of the elements in the application beside. They need to be equal.
   -}
  {- NOTE:  I can do this so easily, because my parser will collapse stacked Type Application in this general shape:
            ((a b) c d) e f
            That will result in (a b c d e f) because they are equivalent and simpler to check.
  -}
  find (Term'T'App term'types)
    = case term'types of
        [] -> return [] -- NOTE: Shouldn't really happen.
        
        (Term'T'Id (Term'Id'Const name) : types) -> do
          syn'env <- ask
          let ts'errs = concat <$> mapM find types
          case syn'env Map.!? name of
            Nothing -> ts'errs
            Just arity -> if arity /= length types
                          then return [Synonym'Not'Fully'Applied name]
                          else ts'errs
        
        types -> concat <$> mapM find types

  find (Term'T'Forall vars (term'preds, term'type))
  --  aliases for Constraints are not allowed -- therefore it's not necessary to check term'preds
    = find term'type


instance Find'Error Term'Constr'Decl where
  find (Con'Decl _ term'types)
    = concat <$> mapM find term'types

  find (Con'Record'Decl _ field'decls)
    = concat <$> mapM (\ (_, term'type) -> find term'type) field'decls
