module REPL.Analyses where


import qualified Data.Map.Strict as Map


import Compiler.Syntax.Term.Declaration ( Term'Decl )
import Compiler.Syntax.ToAST.TranslateState ( Translate'State )
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

import Compiler.Analysis.Syntactic.FixityEnv ( Fixity'Env )
import Compiler.Analysis.Syntactic.SynonymEnv ( Synonym'Env )

import Compiler.Analysis.Semantic.SemanticError ( Semantic'Error(Many'Errors) )

import Compiler.TypeSystem.InferenceEnv ( init'k'env, Kind'Env )


-- NOTE: ragarding the Int part of the result -- follow the trail of it (out of this function) and read the comments if you don't know why it's there
--        In the README there's a comment/idea regarding a more proper implementation.
build'trans'env :: [Term'Decl] -> Translate'State -> (TE.Translate'Env, Translate'State)
build'trans'env declarations tr'state = do
  -- TODO: now I need to run all the analyses, use them to translate to ast
  -- build the environment for the to'ast translation
  -- also to initialize the translation with some initial state, which should be already prepared somewhere
  let fixities :: Fixity'Env
      fixities = Fixity.extract declarations

  let -- (constructors, fields) :: (Constr'Env, Field'Env)
      (constructors, fields) = Constructors.extract declarations


  let user'kind'context :: Kind'Env
      ((user'kind'context, user'class'context), tr'state') = Types.extract declarations tr'state

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

      tr'env = TE.Trans'Env { TE.fixities = fixities
                            , TE.constructors = constructors
                            , TE.fields = fields
                            , TE.kind'context = kind'context
                            , TE.synonyms = synonyms
                            , TE.classes = user'class'context }

  (tr'env, tr'state')


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
