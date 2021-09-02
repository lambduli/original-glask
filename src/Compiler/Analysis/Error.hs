module Compiler.Analysis.Error where


import Data.List


import Compiler.Syntax


data Error
  = Infinite'Type Type Type
  | Infinite'Kind Kind Kind
  | Type'Unif'Mismatch Type Type
  | Kind'Unif'Mismatch Kind Kind
  | Unbound'Var String
  | Unbound'Type'Var String
  | Type'Shape'Mismatch Type Type
  | Kind'Shape'Mismatch Kind Kind
  | Type'Unif'Count'Mismatch [Type] [Type]
  | Kind'Unif'Count'Mismatch [Kind] [Kind]
  | Synonym'Cycle [(String, Type)]

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
    = "Couldn't match type `" ++ show type'a ++ "` with `" ++ show type'b ++ "`"
  
  show (Kind'Unif'Mismatch kind'a kind'b)
    = "Couldn't match kind `" ++ show kind'a ++ "` with `" ++ show kind'b ++ "`"
  
  show (Unbound'Var name)
    = "Unknown variable " ++ name
  
  show (Unbound'Type'Var name)
    = "Unknown type variable " ++ name
  
  show (Type'Shape'Mismatch type'l type'r)
    = "[Shape] Couldn't match type `" ++ show type'l ++ "` with `" ++ show type'r ++ "`"
  
  show (Kind'Shape'Mismatch kind'l kind'r)
    = "[Shape] Couldn't match kind `" ++ show kind'l ++ "` with `" ++ show kind'r ++ "`"
  
  show (Synonym'Cycle aliases)
    = "Found a cycle in the type synonym declaration(s) of\n" ++ intercalate "\n" (map prnt aliases)
      where
        prnt (name, type') = "  type " ++ name ++ " = " ++ show type'
  
  show (Type'Unif'Count'Mismatch _ _) = undefined  -- TODO: FIX this pls!
  
  show (Kind'Unif'Count'Mismatch _ _) = undefined  -- TODO: FIX this pls!

  show (Unexpected s)
    = "Something bad happened: " ++ s