{-# LANGUAGE TypeSynonymInstances #-}

module Compiler.Syntax.ToAST.TranslateState where


import qualified Data.Map.Strict as Map


import Compiler.Counter ( Counter, State(..) )


type Translate'State = Counter


instance State Translate'State where
  get'counter = id
  update'counter counter _ = counter
