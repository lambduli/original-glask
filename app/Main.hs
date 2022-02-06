module Main where

import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra


import Compiler.Parser.Parser (parse'module)

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
import Compiler.Analysis.TypeSystem.InferenceEnv

import Compiler.Syntax.Term
import Compiler.Syntax
import Compiler.Syntax.HasKind

import Compiler.Analysis.Semantic.SemanticError

import Compiler.Analysis.TypeSystem.Program
import Compiler.Analysis.TypeSystem.Type.Infer.Program
import Compiler.Analysis.TypeSystem.Binding
import Compiler.Analysis.TypeSystem.Utils.Infer (close'over)
import Compiler.Analysis.TypeSystem.Infer
import Compiler.Analysis.TypeSystem.InferenceEnv
import Compiler.Analysis.TypeSystem.Solver.Substitution
import Compiler.Analysis.TypeSystem.Solver.Substitutable


main :: IO ()
main = do
  putStrLn "Glamorous Glask REPL."
  putStrLn ""
  load "prelude.glask"


load :: String -> IO ()
load file'name = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  let term'decls = parse contents
  let (trans'env, counter) = build'trans'env term'decls
  
  case do'semantic'analysis term'decls trans'env of
    Left sem'err -> do
      print sem'err
      return ()

    Right () -> do
      case translate'to'ast term'decls counter trans'env of
        Left sem'err -> do
          print sem'err
          return ()

        Right declarations -> do
          -- TODO: now when I have the list of Declarations in AST form
          -- I need to call inference
          -- for the inference I am going to need to build things like class environment and instance environment
          let class'env = Classes.extract declarations


          -- TODO: I need to inclide all bindings in all type classes and instances into the program too
          -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
          -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
          -- then I "just" do the inference
          let program :: Program
              program = to'program declarations
              m'anns = method'annotations program
              type'env = Map.union init't'env $ Map.fromList $ map (second close'over) m'anns


          let TE.Trans'Env{ TE.kind'context = k'env } = trans'env

          -- TODO: Ja potrebuju 
          let infer'env :: Infer'Env
              infer'env = Infer'Env{ kind'env = k'env, type'env = type'env, class'env =  class'env }

          -- (Type'Env, [Constraint Kind])
          case run'infer infer'env (infer'program program) of
            Left err -> do
              print err
            Right (t'env, k'constrs) -> do
              putStrLn "Inference done. ... Maybe ..."


              putStrLn "Program:"
              print program


              putStrLn "Type Environment:"
              print t'env

              -- TODO: I also need to do the Kind inference, probably even before type inference
              -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
              -- for now - I can just infer them together I think
              -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs
              putStrLn "Successfully read the file."
              
              putStrLn "-----------------------"
              
              putStrLn "Class Environment:"
              print class'env
              
              putStrLn "All Declarations:"
              putStrLn $ intercalate "\n" $ map show declarations
              return ()


parse :: String -> [Term'Decl]
parse = parse'module


-- NOTE: ragarding the Int part of the result -- follow the trail of it (out of this function) and read the comments if you don't know why it's there
--        In the README there's a comment/idea regarding a more proper implementation.
build'trans'env :: [Term'Decl] -> (TE.Translate'Env, Int)
build'trans'env declarations = do
  -- TODO: now I need to run all the analyses, use them to translate to ast
  -- build the environment for the to'ast translation
  -- also to initialize the translation with some initial state, which should be already prepared somewhere
  let fixities :: Fixity'Env
      fixities = Fixity.extract declarations

  let -- (constructors, fields) :: (Constr'Env, Field'Env)
      (constructors, fields) = Constructors.extract declarations


  let user'kind'context :: Kind'Env
      (user'kind'context, Types.Counter{ Types.count = count }) = Types.extract declarations
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

  (TE.Trans'Env{ TE.fixities = fixities, TE.constructors = constructors, TE.fields = fields, TE.kind'context = kind'context, TE.synonyms = synonyms }, count)


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


-- NOTE and TODO:
-- The second argument counter ::Int is just a temporary solution to the problem I have ran into.
-- When I want to build a Trans'Env I need to collect all user defined type constructors and assign them a Kind build from fresh Kind Variables
-- BUT, to do that, I need to be inside a State monad. Because I need to be able to increment the counter.
-- On a first glance, it would seem that I could be inside a Translate monad stack, BUT that is not the case, since I am, at that moment, building
-- that very thing (collecting all the parts of the Trans'Env)
-- So I am left with the other choice - I say I am in the context of the State monad which only contains the Counter
-- that means, however, that I will need to pick the final value of the counter AFTER I collect all the type constructors and assign them a Kind
-- and use that as a intial value for the counter in the Translate'State
-- for that exact reason, this parameter needs to be passed through few levels and be used here

-- TODO: this function also needs to merge all the binding groups of the same name together
translate'to'ast :: [Term'Decl] -> Int -> TE.Translate'Env -> Either Semantic'Error [Declaration]
translate'to'ast declarations counter trans'env
  = -- TODO: now to run the translation using some sort of run'X function
  run'translate counter trans'env (to'ast declarations :: Translate [Declaration])


-- NOTE:  this function should be somewhere else
--        I still think, that having to translate from the AST to the Program, Binding'Group and so on, immediately after doing so much work to get the AST is sort of awkward
--        but then again, the AST is not entirely lost, I just replace [Declaration] with Program ([[Explicit], [[Implicit]]] or something like that) which is reasonable
--        the type inference doesn't need to concern itself with other declarations
--        BUT then again - maybe it would be reasonable to merge the process of collecting kind constraints and type constraints on the top level point of view
--        Then I would need to include type declarations into the collection given to the "constraint finding process"
--        so that would maybe mean something like: I give some function the whole [Declaration] collection
--        and IT will split it into a Program and the rest (for type declarations) [I won't need fixity declarations at this point]
--        that would somehow solve my issue with exposing the detail of sorting and transforming to Program on this TOP LEVEL

-- TODO:  I need to collect bindings inside type class declarations and instance declarations too
--        First of -> my current parser doesn't allow type classes to contain a default implementations of methods
--        For the instance bindings ->
--          I should collect type annotations from the type classes
--          I should collect methods (which are strictly untyped)
--          If I combine those into a Map, it would need to be a Map Name (Qualified Type, [Bind'Group])
--            because I have potentially many Bind'Groups per each name/qualified type
--            each name MUST have a qualified type, but it could potentially have no implementations/Bind'Groups
--          The thing is - I don't really need a Map, I don't think I would ever do an explicit lookup, so maybe a Set or a List would be enough
--          But anyway - now I should have all the methods from the instances explicitly typed, so I should just transform it into a "special kind of explicits"
--          special because there is many Bind'Groups per a single type annotations
--          Maybe I could merge all the Bind'Groups into a single Bind'Group to utilize the existing infrastructure for the type analysis
--            I don't think I can do that!
--      !!  I need to 
to'program :: [Declaration] -> Program
to'program decls = Program{ bind'sections = [(explicits, implicits)], methods = methods, method'annotations = m'anns }
  where
    method'annotations :: [(Name, Qualified Type, Name)]
    method'annotations = Method'Annotations.extract decls

    m'anns = map (\ (method'n, q't, _) -> (method'n, q't)) method'annotations

    method'bindings :: [(Name, Bind'Group, Type)]
    method'bindings = Method'Bindings.extract decls

    {-  NOTE:  -}
    methods :: [Method]
    methods = map make'method method'bindings
      where
        make'method :: (Name, Bind'Group, Type) -> Method
        make'method (method'name, bind'group, instance'type) =
          let Just (_, qualified'type, class'var'name) = find (\ (m'n, _, _) -> m'n == method'name) method'annotations -- NOTE: This should always find the result, so the pattern matching on Just (...) should always succeed
              substitution :: Subst T'V Type
              substitution = Sub (Map.singleton (T'V class'var'name (kind instance'type)) instance'type) -- NOTE: the Type Variable must have the same Kind as the Instance Type
              scheme = close'over $ apply substitution qualified'type -- now I have the Type Scheme
          in Method scheme bind'group

 

    annotations :: Map.Map Name (Qualified Type)
    annotations = Annotations.extract decls

    bindings :: Map.Map Name Bind'Group
    bindings = Bindings.extract decls -- NOTE: Myslim, ze tohle jde volat jenom tehdy, kdyz uz jsou vsechny Bind Groups mergnuty do jedne - pokud maji stejne jmeno.

    explicit'map :: Map.Map Name (Qualified Type, Bind'Group)
    explicit'map = Map.intersectionWith (,) annotations bindings

    implicit'map :: Map.Map Name Bind'Group
    implicit'map = Map.difference bindings explicit'map

    explicits = map (\ (q't, b'g) -> Explicit (close'over q't) b'g) $ Map.elems explicit'map

    implicits = map (map Implicit) $ Dependencies.sort $ Map.elems implicit'map
