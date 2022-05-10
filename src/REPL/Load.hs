module REPL.Load where


import System.IO
import Data.List
import qualified Data.Map.Strict as Map
import Data.Tuple.Extra


import Compiler.Counter (Counter, letters)

import Compiler.Parser

import Compiler.Syntax
import Compiler.Syntax.Expression ( Expression(..) )
import Compiler.Syntax.ToAST
-- import Compiler.Syntax.ToAST.TranslateEnv
-- import Compiler.Syntax.ToAST.Translate
-- import Compiler.Syntax.Term
-- import Compiler.Syntax.HasKind
import qualified Compiler.Syntax.ToAST.TranslateEnv as TE

import qualified Compiler.Analysis.Semantic.Synonym.Cycles as Cycles
import qualified Compiler.Analysis.Semantic.Synonym.FullyApplied as Applied

import qualified Compiler.Analysis.Syntactic.Fixity as Fixity
import qualified Compiler.Analysis.Syntactic.Constructors as Constructors
import qualified Compiler.Analysis.Syntactic.Synonyms as Synonyms
import qualified Compiler.Analysis.Syntactic.Types as Types
import qualified Compiler.Analysis.Syntactic.Instance as Instances

import qualified Compiler.Analysis.Syntactic.MethodAnnotations as Method'Annotations
import qualified Compiler.Analysis.Syntactic.MethodBindings as Method'Bindings
import qualified Compiler.Analysis.Syntactic.Annotations as Annotations
import qualified Compiler.Analysis.Syntactic.Bindings as Bindings
import qualified Compiler.Analysis.Syntactic.Class as Classes

import qualified Compiler.Analysis.Semantic.ClassEnv as Class'Env
import qualified Compiler.Analysis.Semantic.Data as Data

-- import Compiler.Analysis.Syntactic.FixityEnv
-- import Compiler.Analysis.Syntactic.FieldEnv
-- import Compiler.Analysis.Syntactic.SynonymEnv
-- import Compiler.Analysis.Syntactic.Types

import Compiler.Analysis.Semantic.SemanticError

import Compiler.TypeSystem.Error

import Compiler.TypeSystem.Program
import Compiler.TypeSystem.Type.Infer.Program
-- import Compiler.TypeSystem.Binding
-- import Compiler.TypeSystem.Utils.Infer
-- import Compiler.TypeSystem.Infer
import Compiler.TypeSystem.InferenceEnv ( Infer'Env(..), init't'env )
import Compiler.TypeSystem.ClassEnv ( Class'Env )

import Compiler.TypeSystem.Solver.Substitution ( Subst(Sub) )
import Compiler.TypeSystem.Binding ( Explicit(..), Implicit(..), Method(Method) )

-- import Compiler.TypeSystem.Solver.Substitutable

import REPL.Repl
import REPL.Analyses
import REPL.Expression
import REPL.Program
import Compiler.Syntax.BindGroup (Bind'Group(Bind'Group))


load :: String -> Counter -> IO ()
load file'name counter = do
  handle <- openFile file'name ReadMode
  contents <- hGetContents handle

  case load'declarations contents counter of
    Left sem'err -> do
      putStrLn $ "Semantic Error: " ++ show sem'err

    Right (decls, trans'env, counter') -> do
      -- putStrLn $ "..................     Prave jsem nacetl deklarace  `load'declarations`   a Counter je " ++ show counter

      -- let Program{ bind'sections = bs, methods = _, method'annotations = m'ans, data'declarations = ds } = make'program decls trans'env counter
      -- putStrLn ""
      -- putStrLn "::::::::::"
      -- putStrLn $ "methods:  " ++ show m'ans
      -- putStrLn ""
      -- putStrLn $ "data'declarations:  " ++ show ds
      -- putStrLn "::::::::::"
      -- putStrLn ""
      -- putStrLn $ "bindings:  " ++ show bs



      -- putStrLn $ "\n\n load declarations    trans'env   " ++ show trans'env
      case process'declarations decls trans'env counter' of
        Left err -> do
          putStrLn $ "Error: " ++ show err
        Right (program, infer'env, trans'env, counter'', class'data, getters) -> do
          putStrLn "Successfully loaded the prelude."
          putStrLn ""
          -- let Program{ bind'sections = bs, methods = ms, method'annotations = m'ans, data'declarations = ds } = program
          putStrLn $ "Program:\n" ++ show program ++ " ...\n\n\n"
          putStrLn $ "Data declarations for Classes: " ++ show class'data ++ " ... \n"
          putStrLn $ "Getters declarations for Classes: " ++ show getters ++ " ... \n"
          putStrLn $ "bind'sections:  " ++ show (bind'section program) ++ " ... \n"
          -- putStrLn $ "\nmethods:  " ++ show ms
          -- putStrLn $ "\nmethod'annotations:  " ++ show m'ans
          -- putStrLn $ "\ndata'declarations:   " ++ show ds

          let k'e = kind'env infer'env

          -- putStrLn $ "\n\n\n type env:  " ++ show (type'env infer'env)
          -- putStrLn $ "Kind Env:  " ++ show k'e
      
          -- putStrLn "Class Environment:"
          -- print class'env

          -- putStrLn "All Declarations:"
          -- putStrLn $ intercalate "\n" $ map show declarations

          -- TODO: I need to generate code for the declarations, stuff like constructors need to be registered and actually all functions need to be registered
          -- there is a lot of code generation that will take place before the REPL can be run
          repl (program, infer'env, trans'env{ TE.kind'context = k'e `Map.union` (TE.kind'context trans'env)}, counter'')


load'declarations :: String -> Counter -> Either Semantic'Error ([Declaration], TE.Translate'Env, Counter)
load'declarations source counter = do
  let term'decls = parse'module source
  let (trans'env, counter') = build'trans'env term'decls counter

  do'semantic'analysis term'decls trans'env

  (declarations, counter'') <- translate term'decls counter' trans'env

  return (declarations, trans'env, counter'')


make'program :: [Declaration] -> TE.Translate'Env -> Counter -> Program
make'program declarations trans'env counter =
  -- TODO: now when I have the list of Declarations in AST form
  -- I need to call inference
  -- for the inference I am going to need to build things like class environment and instance environment
  let class'env = Class'Env.extract declarations


  -- TODO: I need to extract `Type Assumptions` about all data constructors in the list of Declarations
      constr'assumptions = Data.extract declarations


  -- TODO: I need to inclide all bindings in all type classes and instances into the program too
  -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
  -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
  -- then I "just" do the inference
      program :: Program
      program = to'program declarations

      -- m'anns = method'annotations program

  in program


process'declarations :: [Declaration] -> TE.Translate'Env -> Counter -> Either Error (Program, Infer'Env, TE.Translate'Env, Counter, [Data], [Declaration])
process'declarations declarations trans'env counter = do
  -- for the inference I am going to need to build things like class environment and instance environment
  let class'env = Class'Env.extract declarations

  let classes = Classes.extract declarations
  -- list of
    --  Class { class'name :: Name, class'param :: T'V', class'supers :: [Predicate], class'declarations :: [Declaration] }
  -- TODO: I also need to transform those into data declarations


  -- TODO: I need to build overloaded
  -- overloaded contains three kinds of associations
  -- overloads - overloaded functions/constants
    -- these are two kinds, either implicitly typed or explicitly typed
    -- implicits are now taken care of in the function which infers groups
    -- explicits could be registered when they are put into the typing context - done
  -- methods
    -- I can also take care of it in one of that infer'types function - done
  -- recursive
    -- those are kind strange, but should be taken care of by the infer'impls
    -- at the beginning of it, each mutual binding gets free type variable and association of recursive
    -- the association only lasts for current scope and is later replaced with proper overload (might) if it has a qualified type by the group function
  -- so in fact, here I don't need to do anything


  -- TODO: To build `instance'env`
  -- I can use it to store all the instances there.
  let instances   = Instances.extract declarations
      -- TODO: Problem je, ze to co tady seberu, tak pak neprojde elaboraci, takze metody, ktery registruju a pouzivam do dictionaries
      -- nejsou elaborovany, to je velky spatny
      -- takze otazka je - jestli jsem schopnej misto tehle, pouzit pak ty elaborovany az po tom co probehne cela analyza?
      -- nebo muzu upravit to jak pracuju s metodama
      -- reknu ze metody budou prave takhle zaregistrovany do globalniho prostoru pod unikatnima identifikatorama, ktery okamzite pouziju
      -- a pak misto toho abych bral metody tak jak beru, tak je budu brat jeste vic jako explicity - kazda metoda bude mit svoji promennou
      -- otazka je - kdy presne se deje takova ta saskarna jak prochazim instance/classy
      -- a sbiram typovy anotace a pak cachruju s contextama aby metody mely spravnou tupovou anotaci se spravnym kontextem, je to ted, nebo to je az pak?
      -- idealne totiz byc bylo nejlepsi pouzit to co uz mam a annotovat tema typama tyhle metodovy promenny
      -- no nebo tak neco?
      --
      --
      hexa'tuples  = map to'hexa'tuple instances
      to'hexa'tuple :: (Instance, [Declaration]) -> ((Name, Type), (Name, [Predicate], Predicate, Declaration))
      to'hexa'tuple ((context :=> p@(Is'In cl'name type')), decls) =
        let ty'const@(T'Con (T'C ty'name _)) = case type' of
                                                T'Var' t' -> error "illegal syntactically"
                                                T'Meta m'v -> error "illegal syntactically"
                                                T'Con (T'C name _) -> type'
                                                T'Tuple tys -> error "not supporting tuples now"
                                                -- TODO: but later it would be the specific constructor for the tuple, like `(,)` for a pair
                                                T'App ty _ -> get'ty'const ty
                                                T'Forall t's qual -> error "illegal syntactically"
            dictionary'name = "d-" ++ cl'name ++ "-" ++ ty'name
            decl' = make'decl dictionary'name cl'name context decls
        -- ((Type Class Name, Type Constant Name), (dictionary global variable name, instance context, the whole instance head, declaration of the dictionary/dict-constructor function))
        in ((cl'name, ty'const), (dictionary'name, context, p, decl'))
      
      make'decl :: Name -> Name -> [Predicate] -> [Declaration] -> Declaration
      make'decl dict'name cl'name [] decls
      -- TODO: if the context is empty, I only create a variable and call corresponding constructor of the data type representing the class (which is not defined yet, but soon it will be)
        = let cons  = Const cl'name
              names = map (to'method dict'name) decls
              vars  = map Var names
              expr  = foldl App cons vars
              match = Match{ patterns = [], rhs = expr }
              bind  = Binding (Bind'Group{ name = dict'name, alternatives = [match] })
          in bind
      make'decl dict'name cl'name context decls
      -- TODO: If the context is non-empty, I create a function, which will take as many values as there are predicates in the context
      -- it's body will be an application, similar as above, but each argument to the constructor won't be just a variable
      -- instead they will be those variables (names of specific methods) applied to all the extra parameters (dictionaries)
      -- and that's just it
        = let cons    = Const cl'name
              d'names = take (length context) letters
              m'names = map (to'method dict'name) decls
              params  = map P'Var d'names
              vars    = map Var d'names
              methods = map (\ n -> foldl App (Var n) vars) m'names
              expr    = foldl App cons methods
              matches = [Match { patterns = params, rhs = expr }]
              bind    = Binding $ Bind'Group{ name = dict'name, alternatives = matches }
          in bind

      
      to'method :: Name -> Declaration -> Name
      to'method dict'name (Binding bg@Bind'Group{ name = n })
        = let name' = dict'name ++ "-" ++ n
              -- decl = Binding (bg{ name = name' })
          in name'

      get'ty'const :: Type -> Type
      get'ty'const (T'App ty _) = get'ty'const ty
      get'ty'const ty@(T'Con (T'C name _)) = ty
      get'ty'const _ = error "shouldn't have happened"
      -- TODO: I need to create the values and register them in the global scope (put them here)
      -- some of them will be super simple
      -- because they will just be instances
      -- but some of them will be functions, which create the value by partially applying all the methods to all its arguments
      -- I think I might be able to just create them here, since I already have the names and I strip some of the values from those tuples anyway



  -- now I just trim the the pentas to only associate to the name of the global variable
      insts = map (\ (x, (n, _, _, _)) -> (x, n)) hexa'tuples

      extra'declarations = map (\ (_, (_, _, _, decl)) -> decl) hexa'tuples

      penta'tuples = map (\ (x, (a, b, c, _)) -> (x, (a, b, c))) hexa'tuples



  -- TODO: I need to extract `Type Assumptions` about all data constructors in the list of Declarations
  let constr'assumptions = Data.extract declarations


  -- TODO: I need to inclide all bindings in all type classes and instances into the program too
  -- I need to split binding declarations into - explicitly typed (also includes instance bindings) and implicitly typed
  -- then I need to do the dependency analysis on those two groups and figure out the order in which I will infer them
  -- then I "just" do the inference
  let program :: Program
      program = to'program declarations
      m'anns = method'annotations program
      type'env = init't'env `Map.union` (Map.fromList constr'assumptions) `Map.union` (Map.fromList $ map (\ (name, type', _) -> (name, type')) m'anns) -- this maping is dirty, but I need just method name and its type

  let TE.Trans'Env{ TE.kind'context = k'env, TE.classes = class'ctxt } = trans'env

  let infer'env :: Infer'Env
      infer'env = Infer'Env { kind'env = k'env
                            , type'env = type'env
                            , class'env =  class'env
                            , constraint'env = class'ctxt
                            , kind'substitution = Sub Map.empty
                            , instance'env = insts -- [((Name, Type), Name)] ||| (Name, Type) is the kind of information in the placeholder, like Num Int, or Eq []
                            , overloaded = [] -- [(Name, Overloaded)]
                            , instances = penta'tuples }

  -- (Type'Env, [Constraint Kind])
  -- (t'env, k'constr) <- run'infer infer'env (infer'program program)

  (program', t'env', k'env', c'env, cnt, cl'env) <- infer'whole'program program infer'env counter


  -- TODO: I also need to do the Kind inference, probably even before type inference
  -- figure out the order in which I need to infer the Kinds of `data` and `type` declarations
  -- for now - I can just infer them together I think
  -- but later I could implement Kind Polymorphism --> I would need to first top sort them into SCCs

  -- NOTE:  I need to merge the alread-known with the newly-inferred
  --        In essence - built-in typing context + method types merging with inferred Implicits + checked Explicits
  -- let type'env' = t'env `Map.union` type'env
  --        I no longer need to do this. I made the `infer'whole'program` apply the kind substitution to the both "base type environment" and the "inferred env from the assumptions"
  --        and union them and return it

  -- NOTE: Now I add all the instance dictionaries and dictionary-constructors into the program
  let pr@Program{ bind'section = (explicits, implicits's), methods = methods } = program'
  let extra'implicits = map (\ (Binding bg) -> Implicit bg) extra'declarations
  -- TODO: I also need to convert methods into implicits (at this point the type annotation would be useless)
  -- Method Sigma'Type Bind'Group Name Name
  let method'declarations =
        map
          (\ (Method sigma b'g cl'name m'name) -> Explicit sigma (b'g{ name = m'name }))
              -- [ Signature (T'Signature m'name sigma)
              -- , Binding (b'g{ name = m'name })  -- I am just switching the name, rest stays the same
              -- ]) -- TODO: the this method needs a getter from the dictionary, or maybe I will define it together with the data declaration for the class-data-dictionary
          methods
  let new'program = pr{ bind'section = (explicits ++ method'declarations, implicits's ++ [extra'implicits]) }
  -- extra'implicits are the dictionaries themselves - variable binding with the right hand side being a construction of the dictionary (later it might be a function too)
  -- method'declarations are global bindings of methods with unique names

  -- TODO: ted je potreba vyrobit data deklarace pro vsechny ty tridy
  -- na vyssi urovni z nich pak budu muset vygenerovat curried constructory, ale to by se snad melo dit az v kroku kdy budu delat generaci do core
  let sign'type :: Declaration -> Sigma'Type
      sign'type (Signature (T'Signature _ sigma)) = sigma
      
      make'class :: Class -> (Data, [Declaration])
      make'class Class{ class'name = cl'name, class'param = cl'par, class'supers = supers, class'declarations = decls }
        = let the'name = "x"
              the'body = Var the'name
              patterns = replicate (length decls) P'Wild -- only wildcards
              matches = map (\ indx -> let (front, _ : rear) = splitAt indx patterns in Match{ patterns = [P'Con cl'name $ front ++ [P'Var the'name] ++ rear], rhs = the'body }) [0 .. length decls - 1]
              getters = zipWith (\ (Signature (T'Signature name _)) match -> Binding $ Bind'Group{ name = name, alternatives = [match] }) decls matches
          
              d = Data{ type'name = T'C cl'name (K'Star `K'Arr` K'Star), type'params = [cl'par], constructors = [Con'Decl cl'name (map sign'type decls)] }
          in (d, getters)

      (data', getterss) = unzip $ map make'class classes
      getters' = concat getterss

      
      --
      -- decls are type signatures, I need to just collect those types
      -- supers are going to be ignored for now
      -- cl'par is important, that's going to be a data parameter
      -- cl'name will become type name

  -- Data { type'name :: T'C, type'params :: [T'V'], constructors :: [Constr'Decl] }
  -- and this I have
  -- Class { class'name :: Name, class'param :: T'V', class'supers :: [Predicate], class'declarations :: [Declaration] }


  return (new'program, infer'env{ type'env = t'env', kind'env = k'env', constraint'env = c'env, class'env = cl'env }, trans'env, cnt, data', getters')
