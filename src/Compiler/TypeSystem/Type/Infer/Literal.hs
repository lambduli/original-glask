module Compiler.TypeSystem.Type.Infer.Literal where


import Compiler.Counter

import Compiler.Syntax

import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.Type.Constants

import Compiler.TypeSystem.Utils.Infer


infer'lit :: Literal -> Infer ([Predicate], Type)
infer'lit (Lit'Int _) = do
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  return ([Is'In "Num" t'var], t'var)

infer'lit (Lit'Double double) = do
  fresh'name <- fresh
  let t'var = T'Var (T'V fresh'name K'Star)
  return ([Is'In "Fractional" t'var], t'var)

infer'lit (Lit'Char char) = do
  return ([], t'Char)
  