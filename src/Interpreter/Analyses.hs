module Interpreter.Analyses where


import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra


import Compiler.Syntax.ToAST.Translate
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles
import qualified Compiler.Analysis.Semantic.Synonym.FullyApplied as Applied

import qualified Compiler.Analysis.Syntactic.Fixity as Fixity
import qualified Compiler.Analysis.Syntactic.Constructors as Constructors
import qualified Compiler.Analysis.Syntactic.Synonyms as Synonyms
import qualified Compiler.Analysis.Syntactic.Types as Types

import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings

import qualified Compiler.Analysis.Semantic.DependencyAnalysis as Dependencies
import qualified Compiler.Analysis.Semantic.Class as Classes

import Compiler.Syntax.ToAST

import Compiler.Analysis.Syntactic.FixityEnv
import Compiler.Analysis.Syntactic.FieldEnv
import Compiler.Analysis.Syntactic.SynonymEnv
import Compiler.TypeSystem.InferenceEnv

import Compiler.Syntax.Term
import Compiler.Syntax
import Compiler.Syntax.HasKind

import Compiler.Analysis.Semantic.SemanticError

import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Type.Infer.Program
import Compiler.TypeSystem.Binding
import Compiler.TypeSystem.Utils.Infer
import Compiler.TypeSystem.Infer

import Compiler.TypeSystem.Solver.Substitution
import Compiler.TypeSystem.Solver.Substitutable




-- NOTE: ragarding the Int part of the result -- follow the trail of it (out of this function) and read the comments if you don't know why it's there
--        In the README there's a comment/idea regarding a more proper implementation.
build'trans'env :: [Term'Decl] -> (TE.Translate'Env, Types.Counter)
build'trans'env declarations = do
  -- TODO: now I need to run all the analyses, use them to translate to ast
  -- build the environment for the to'ast translation
  -- also to initialize the translation with some initial state, which should be already prepared somewhere
  let fixities :: Fixity'Env
      fixities = Fixity.extract declarations

  let -- (constructors, fields) :: (Constr'Env, Field'Env)
      (constructors, fields) = Constructors.extract declarations


  let user'kind'context :: Kind'Env
      (user'kind'context, counter) = Types.extract declarations
  -- TODO: now the `count` should be used to initialize the counter in the Translate'State

  let kind'context :: Kind'Env
      kind'context = init'k'env `Map.union` user'kind'context
      -- TODO: this should not be empty, it needs to contain kinds of all known type constructors
      -- that means primitive types like Int, Char, Tuple, Unit, List, Bool, (->)
      -- am I missing something?
      -- YES! It also needs to contain assignments for all user declared types
      -- TODO: add all user defined types declared with `data` keyword
      -- Think about type synonyms, there are at least two ways to go about them
      -- I can ignore them in this step, because they will be fully erased in the following step
      -- so (for instance) the result of `to'ast` will not contain any type synonym declarations and the code will not too
      -- OR I keep the synonyms bit longer
      -- but honestly, I dont' really see the value in that
      -- since I already do Fully Applied and Cycle analyses on the Term level
      -- I don't think it's necessary to translate synonyms to AST too

  let synonyms :: Synonym'Env
      synonyms = Synonyms.extract declarations

  (TE.Trans'Env{ TE.fixities = fixities, TE.constructors = constructors, TE.fields = fields, TE.kind'context = kind'context, TE.synonyms = synonyms }, counter)


-- TODO: implement checking that every declaration which needs to be unique is in fact unique
--        like no types declared multiple times, same with functions and synonyms, ...
do'semantic'analysis :: [Term'Decl] -> TE.Translate'Env -> Either Semantic'Error ()
do'semantic'analysis declarations TE.Trans'Env{ TE.synonyms = synonyms } = do
  -- TODO: I can start with semantic analysis
  let errors = Applied.check synonyms declarations
  raise errors -- only of there are any

  let errors = Cycles.analyze synonyms declarations
  raise errors


raise :: [Semantic'Error] -> Either Semantic'Error ()
raise [] = Right ()
raise errors = Left $ Many'Errors errors
