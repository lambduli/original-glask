module Compiler.Analysis.TypeSystem.Type.Infer.Literal where


import Compiler.Syntax

import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.Type.Constants

import Compiler.Analysis.TypeSystem.Utils.Infer


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
  