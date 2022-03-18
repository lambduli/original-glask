module Compiler.TypeSystem.Error where


import Compiler.Syntax.Kind ( Kind )
import Compiler.Syntax.Name ( Name )
import {-# SOURCE #-} Compiler.Syntax.Type ( Type, M'V )
import Compiler.Syntax.HasKind (HasKind(kind))


data Error
  = Infinite'Type Type Type
  | Infinite'Kind Kind Kind
  | Type'Unif'Mismatch Type Type
  | Kind'Unif'Mismatch Kind Kind
  | Unbound'Var Name
  | Unbound'Type'Var String
  | Type'Shape'Mismatch Type Type
  | Kind'Shape'Mismatch Kind Kind
  | Type'Unif'Count'Mismatch [Type] [Type]
  | Kind'Unif'Count'Mismatch [Kind] [Kind]
  | Signature'Too'General Type Type
  | Context'Too'Weak
  | Impredicative M'V Type -- Error reported when predicativity assumption is to be broken. (Unifying type variable with sigma type.)

  | Unexpected String -- this is just temporary helper constructor for me to debug stuff ... mostly
  deriving (Eq)


instance Show Error where
  show (Infinite'Type type'a type'b)
    = "Occurs check: cannot construct the infinite type:\n  "
      ++ show type'a ++ " ~ " ++ show type'b
  
  show (Infinite'Kind kind'a kind'b)
    = "Occurs check: cannot construct the infinite kind:\n  "
      ++ show kind'a ++ " ~ " ++ show kind'b
  
  show (Type'Unif'Mismatch type'a type'b)
    = "Couldn't match type `" ++ show type'a ++ " :: " ++ show (kind type'a) ++ "` with `" ++ show type'b ++ "` :: " ++ show (kind type'b)
  
  show (Kind'Unif'Mismatch kind'a kind'b)
    = "Couldn't match kind `" ++ show kind'a ++ "` with `" ++ show kind'b ++ "`"
  
  show (Unbound'Var name)
    = "Unknown variable " ++ name
  
  show (Unbound'Type'Var name)
    = "Unknown type variable " ++ name
  
  show (Type'Shape'Mismatch type'l type'r)
    = "[Shape] Couldn't match type `" ++ show type'l ++ "` :: " ++ show (kind type'l) ++ " with `" ++ show type'r ++ "` :: " ++ show (kind type'r)
  
  show (Kind'Shape'Mismatch kind'l kind'r)
    = "[Shape] Couldn't match kind `" ++ show kind'l ++ "` with `" ++ show kind'r ++ "`"

  show (Type'Unif'Count'Mismatch _ _) = undefined  -- TODO: FIX this pls!
  
  show (Kind'Unif'Count'Mismatch _ _) = undefined  -- TODO: FIX this pls!
  
  show (Signature'Too'General offered inferred)
    = "Signature is too general:\n    offered type: '" ++ show offered ++ "'\n    inferred type: '" ++ show inferred ++ "'"

  show (Context'Too'Weak)
    = "Context is too weak" -- TODO: add some info about what am I talking about too

  show (Impredicative t'var type')
    = "Impredicative type - type variable '" ++ show t'var ++ "' can't be unified with the poly type '" ++ show type' ++ "'."

  show (Unexpected s)
    = "Something bad happened: " ++ s