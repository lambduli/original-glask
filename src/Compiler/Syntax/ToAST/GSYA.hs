{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Compiler.Syntax.ToAST.GSYA where

import Prelude hiding ( Left, Right )

import qualified Data.Map.Strict as Map
import Control.Monad.Trans.Reader ( asks, local )
import Control.Monad.Except ( MonadError(throwError) )
import Data.Sequence ( Seq, (<|), (|>), (><), empty, pattern Empty, pattern (:|>), pattern (:<|) )
import Data.Foldable ( Foldable(toList) )


import Compiler.Syntax.ToAST.TranslateEnv ( Translate'Env(fixities) )
import Compiler.Syntax.Term.Expression ( Term'Expr (..) )
import Compiler.Syntax.Term.Pattern ( Term'Pat(..) )
import Compiler.Syntax.ToAST.Translate ( run'translate, Translate )
import Compiler.Syntax.Term.Identifier (Term'Id(Term'Id'Var, Term'Id'Const))
import Compiler.Syntax.Fixity ( Fixity(..) )
import Compiler.Syntax.Associativity ( Associativity(Left, None, Right) )

import Compiler.Syntax.ToAST.GSYA.Token ( Token(..), to'op )


import Compiler.Analysis.Semantic.SemanticError ( Semantic'Error(..), GSYA'Error(..) )


data State a  = State{ output :: Seq (Token a), op'stack :: [Token a], app'qu :: Seq (Token a) }


{-  The GSYA class is an implementation of Extended Shunting Yard Algorithm

    type parameter `a` is a type of the input sequence member
-}

-- TODO: I need to pass the information about precedences, associativities and fixities
class GSYA a where
  process :: [Token a] -> Translate [Token a]

  to'token :: a -> Translate (Token a)

  explicit'app :: Token a

  make'app'explicit :: [Token a] -> [Token a]

  disambiguate'minus :: [Token a] -> [Token a]


instance GSYA Term'Expr where
  to'token term@(Term'E'Op t'op) = do
    fix'env <- asks fixities

    let name = case t'op of
                Term'Id'Var n -> n
                Term'Id'Const n -> n

    (f, a, p) <- case fix'env Map.!? name of
      Nothing -> throwError $ Not'In'Scope'Data name
      Just info -> return info

    return $ Operator{ fixity = f, associativity = a, precedence = p, term = term }

  to'token term
    = return $ Term term

  
  explicit'app = Operator{ fixity = Infix, associativity = Left, precedence = 10, term = Term'E'Op (Term'Id'Var "@") }

  -- [ fn'om, arg, +, fn'om, arg ]

  make'app'explicit [] = []

  -- insert @ before all tokens except infix and postfix operator - when the last token was "non-operator" or postfix operator
  make'app'explicit (t1 : o2@Operator{ fixity = Infix } : rest)
    = t1 : make'app'explicit (o2 : rest)

  make'app'explicit (t1 : o2@Operator{ fixity = Postfix } : rest)
    = t1 : make'app'explicit (o2 : rest)

  make'app'explicit (t1@Operator{ fixity = Postfix } : t2 : rest)
    = t1 : explicit'app : make'app'explicit (t2 : rest)

  make'app'explicit (t1@(Term _) : t2@(Term _) : rest)
    = t1 : explicit'app : make'app'explicit (t2 : rest)

  -- TODO:  I am not quite sure this case is correct, so check it later ideally.
  make'app'explicit (t1 : rest)
    = t1 : make'app'explicit rest


  disambiguate'minus [] = []

  disambiguate'minus (o1@Operator{ fixity = Infix } : o2@Operator{ term = Term'E'Op (Term'Id'Var "-") } : rest)
    = o1 : disambiguate'minus (o2{ fixity = Prefix } : rest)

  disambiguate'minus (o1@Operator{ fixity = Prefix } : o2@Operator{ term = Term'E'Op (Term'Id'Var "-") } : rest)
    = o1 : disambiguate'minus (o2{ fixity = Prefix } : rest)

  disambiguate'minus (t1 : rest)
    = t1 : disambiguate'minus rest


  process tokens = run tokens init
    where
      init :: State a
      init = State{ output = empty, op'stack = [] }

      -- run :: [Token a] -> State a -> Translate [Token a]
      run :: [Token Term'Expr] -> State Term'Expr -> Translate [Token Term'Expr]
      run [] State{ output = out, op'stack = op's }
        = return $ reverse $ toList out ++ op's
      
      run (t1@(Term t) : rest) state@State{ output = out }
        = run rest state{ output = out |> t1}
      
      run (op@Operator{ fixity = Prefix } : rest) state@State{ op'stack = op'stack }
        = run rest state{ op'stack = op : op'stack }
      
      run (o1@Operator{ fixity = Infix, precedence = prec'o1, associativity = assoc'o1 } : rest) state
        = go state >>= run rest
          where
            -- go :: State a -> Translate (State a)
            go :: State Term'Expr -> Translate (State Term'Expr)
            go state@State{ op'stack = [] }
              = return state{ op'stack = [o1] }
            
            go state@State{ op'stack = o2@Operator{ fixity = Postfix } : op'stack, output = out }
              = go state{ output = out |> o2, op'stack = op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack }
              | prec'o2 < prec'o1 = return state{ op'stack = o1 : o2 : op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack, output = out }
              | prec'o2 > prec'o1 = go state{ output = out |> o2, op'stack = op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2, associativity = assoc'o2 } : op'stack, output = out }
              | prec'o2 == prec'o1 = case (assoc'o1, assoc'o2) of
                                      (None, None) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)
                                      (Left, Left) -> go state{ op'stack = op'stack, output = out |> o2 }
                                      (Right, Right) -> return state{ op'stack = o1 : o2 : op'stack }
                                      (_, _) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)

            go _ = error "Internal Error - GSYA - O1 is INFIX and there's something strange going on."

      run (o1@Operator{ fixity = Postfix, precedence = prec'o1, associativity = assoc'o1 } : rest ) state
        = go state >>= run rest
          where
            -- go :: State a -> Translate (State a)
            go :: State Term'Expr -> Translate (State Term'Expr)
            go state@State{ op'stack = [] }
              = return state{ op'stack = [o1] }
            
            go state@State{ op'stack = o2@Operator{ fixity = Postfix } : op'stack, output = out }
              = go state{ output = out |> o2, op'stack = op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack }
              | prec'o2 < prec'o1 = return state{ op'stack = o1 : o2 : op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack, output = out }
              | prec'o2 > prec'o1 = go state{ output = out |> o2, op'stack = op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2, associativity = assoc'o2 } : op'stack, output = out }
              | prec'o2 == prec'o1 = case (assoc'o1, assoc'o2) of
                                      (None, None) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)
                                      (Left, Left) -> go state{ op'stack = op'stack, output = out |> o2 }
                                      (Right, Right) -> return state{ op'stack = o1 : o2 : op'stack }
                                      (_, _) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)

            go _ = error "Internal Error - GSYA - O1 is Postfix and there's something strange going on."




instance GSYA Term'Pat where
  to'token term@(Term'P'Op t'op) = do
    fix'env <- asks fixities

    let name = case t'op of
                Term'Id'Var n -> n
                Term'Id'Const n -> n

    (f, a, p) <- case fix'env Map.!? name of
      Nothing -> throwError $ Not'In'Scope'Data name
      Just info -> return info

    return $ Operator{ fixity = f, associativity = a, precedence = p, term = term }

  to'token term
    = return $ Term term


  disambiguate'minus [] = []

  -- I am not even sure this will ever happen, it should get eaten by the lexer already and become negative literal or whatever
  disambiguate'minus (o1 : o2@Operator{ term = Term'P'Op (Term'Id'Var "-") } : rest)
    = o1 : disambiguate'minus (o2{ fixity = Prefix } : rest)

  disambiguate'minus (t1 : rest)
    = t1 : disambiguate'minus rest

  
  process tokens = run tokens init
    where
      flush'app'qu :: Seq (Token Term'Pat) -> Seq (Token Term'Pat) -> Translate (Seq (Token Term'Pat))
      flush'app'qu Empty out = return out
      flush'app'qu ((Term (Term'P'Id (Term'Id'Const con'name))) :<| args) out = do
        let args' = map (\ (Term t) -> t) $ toList args
        return $ out |> Term (Term'P'Con con'name args')
      flush'app'qu _ _ = do
        throwError $ Illegal "constructor patter must start with constructor"

      init :: State a
      init = State{ output = empty, op'stack = [], app'qu = empty }

      run :: [Token Term'Pat] -> State Term'Pat -> Translate [Token Term'Pat]
      run [] State{ output = out, op'stack = op's }
        = return $ reverse $ toList out ++ op's
      
      run (t1@(Term t) : rest) state@State{ app'qu = app'qu }
        = run rest state{ app'qu = app'qu |> t1 }
      
      -- run (op@Operator{ fixity = Prefix } : rest) state@State{ op'stack = op'stack, app'qu = app'qu, output = out } = do
      --   out <- flush'app'qu app'qu out
      --   run rest state{ op'stack = op : op'stack, app'qu = empty, output = out }
      
      run (o1@Operator{ fixity = Infix, precedence = prec'o1, associativity = assoc'o1 } : rest) state@State{ app'qu = app'qu, output = out } = do
        out <- flush'app'qu app'qu out
        go state{ app'qu = empty, output = out } >>= run rest
          where
            go :: State Term'Pat -> Translate (State Term'Pat)
            go state@State{ op'stack = [] }
              = return state{ op'stack = [o1] }
            
            -- go state@State{ op'stack = o2@Operator{ fixity = Postfix } : op'stack, output = out }
            --   = go state{ output = out |> o2, op'stack = op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack }
              | prec'o2 < prec'o1 = return state{ op'stack = o1 : o2 : op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack, output = out }
              | prec'o2 > prec'o1 = go state{ output = out |> o2, op'stack = op'stack }
            
            go state@State{ op'stack = o2@Operator{ precedence = prec'o2, associativity = assoc'o2 } : op'stack, output = out }
              | prec'o2 == prec'o1 = case (assoc'o1, assoc'o2) of
                                      (None, None) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)
                                      (Left, Left) -> go state{ op'stack = op'stack, output = out |> o2 }
                                      (Right, Right) -> return state{ op'stack = o1 : o2 : op'stack }
                                      (_, _) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)

            go _ = error "Internal Error - GSYA - O1 is INFIX and there's something strange going on."

      -- run (o1@Operator{ fixity = Postfix, precedence = prec'o1, associativity = assoc'o1 } : rest ) state@State{ app'qu = app'qu, output = out } = do
      --   out <- flush'app'qu app'qu out
      --   go state{ app'qu = empty, output = out } >>= run rest
      --     where
      --       go :: State Term'Pat -> Translate (State Term'Pat)
      --       go state@State{ op'stack = [] }
      --         = return state{ op'stack = [o1] }
            
      --       go state@State{ op'stack = o2@Operator{ fixity = Postfix } : op'stack, output = out }
      --         = go state{ output = out |> o2, op'stack = op'stack }
            
      --       go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack }
      --         | prec'o2 < prec'o1 = return state{ op'stack = o1 : o2 : op'stack }
            
      --       go state@State{ op'stack = o2@Operator{ precedence = prec'o2 } : op'stack, output = out }
      --         | prec'o2 > prec'o1 = go state{ output = out |> o2, op'stack = op'stack }
            
      --       go state@State{ op'stack = o2@Operator{ precedence = prec'o2, associativity = assoc'o2 } : op'stack, output = out }
      --         | prec'o2 == prec'o1 = case (assoc'o1, assoc'o2) of
      --                                 (None, None) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)
      --                                 (Left, Left) -> go state{ op'stack = op'stack, output = out |> o2 }
      --                                 (Right, Right) -> return state{ op'stack = o1 : o2 : op'stack }
      --                                 (_, _) -> throwError $ GSYA $ Mixing (to'op o1) (to'op o2)

      --       go _ = error "Internal Error - GSYA - O1 is Postfix and there's something strange going on."

      run _ _ = do
        throwError $ Illegal "there is probably PRE/POST-fix constructor operator in the pattern"
