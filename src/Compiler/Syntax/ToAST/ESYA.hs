-- {-# LANGUAGE MultiParamTypeClasses #-}

module Compiler.Syntax.ToAST.ESYA where


-- import Compiler.Syntax.ToAST.Translate

import Compiler.Syntax.Term



{-  The ESYA class is an implementation of Extended Shunting Yard Algorithm

    type parameter `a` is a type of the input sequence member
-}

-- TODO: I need to pass the information about precedences, associativities and fixities
class ESYA a where
  process :: [a] -> [a]
  process as = do
    undefined
  -- TODO: implement the ESYA here

  is'value :: a -> Bool
  is'value = not . is'operator

  is'operator :: a -> Bool
  is'operator = not . is'value


instance ESYA Term'Pat where
  is'value (Term'P'Id _) = True
  is'value (Term'P'Op _) = False
  is'value (Term'P'Lit _) = True
  is'value (Term'P'App _) = True
  is'value (Term'P'Labeled _ _) = True
  is'value (Term'P'Tuple _) = True
  is'value (Term'P'List _) = True
  is'value (Term'P'As _ _) = True
  is'value Term'P'Wild = True
