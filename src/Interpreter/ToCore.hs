module Interpreter.ToCore where


import Data.Maybe ( mapMaybe )


import Compiler.Counter ( letters )

import Compiler.Syntax.Name ( Name )
import qualified Compiler.Syntax.Declaration as AST
import qualified Compiler.Syntax.Expression as AST
import qualified Compiler.Syntax.Match as AST
import Compiler.Syntax.Pattern ( Pattern(P'Con, P'Wild) )
import qualified Compiler.Syntax.BindGroup as AST

import Interpreter.Core ( Core(..), Binding(..), Match(..) )
import Interpreter.Error ( Evaluation'Error(Non'Exhaustive) )
import Compiler.TypeSystem.BindSection (Bind'Section)
import Compiler.TypeSystem.Binding (Explicit(Explicit), Implicit (Implicit))
import Data.List (mapAccumR)


to'core :: AST.Expression -> Core
to'core (AST.Var name)
  = if is'prim'op name then Prim'Op name else Var name

to'core (AST.Const name)
  = if is'prim'op name then Prim'Op name else Var name

to'core (AST.Op name)
  = if is'prim'op name then Prim'Op name else Var name

to'core (AST.Lit lit)
  = Lit lit

to'core (AST.Abs pattern expr)
  = let (patterns, body)  = decompose expr
        body'             = to'core body
        all'patterns      = pattern : patterns

        vars'patts        = zip (take (length all'patterns) $ map ("__" ++) letters) all'patterns
        vars              = map fst vars'patts

        -- TODO: now I need to build the CASE expression
        -- strategy:
        -- for each (pattern, var) in the list of vars'patts
        -- I create a case with two branches - one is doing something, the other is error boundary
        -- its going to be case <the name of the var from the pair> of
        --                   <pattern from the pair> -> <<recursion>>
        --                   <error boundary> -> errror# Non'Exhaustive
        -- so it seems like fold would be optimal
        -- right fold - because the final part of the CASE needs to hold the BODY
        -- OK
        case'             = build'case vars'patts body'

        -- TODO: now I fold all the vars into a lot of nested lambdas - the last lambda has a CASE body ; right fold
        lambda            = foldr Abs case' vars
        
        decompose :: AST.Expression -> ([Pattern], AST.Expression)
        decompose (AST.Abs p b) = let (ps, body) = decompose b in (p : ps, body)
        decompose expr = ([], expr)

        build'case :: [(Name, Pattern)] -> Core -> Core
        build'case pairs expr = foldr build' expr pairs
          where
            build' (n, p) e = let the'match       = Match{ patterns = [p], rhs = e }
                                  error'boundary  = Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive }
                              in  Case (Var n) [the'match, error'boundary]

    in  lambda

to'core (AST.App fn arg)
  = App (to'core fn) (to'core arg)

to'core (AST.Infix'App left op right)
  = App (App (to'core op) (to'core left)) (to'core right)

to'core (AST.Tuple expressions)
  = Tuple (map to'core expressions)

to'core (AST.If cond then' else')
  = let motive = to'core cond
        then'match = Match{ patterns = [P'Con "True" []], rhs = to'core then' }
        else'match = Match{ patterns = [P'Con "False" []], rhs = to'core else' }
        error'match = Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive }
        case' = Case motive [then'match, else'match, error'match]
    in  case'

to'core (AST.Let decls expr)
  = let bindings  = decls'to'core decls
        body      = to'core expr
        let'      = Let bindings body
    in  let'

to'core (AST.Ann expr _)
  = to'core expr

to'core (AST.Case motive'e matches'e)
  = let motive  = to'core motive'e
        matches = map match'to'core matches'e
        case'   = Case motive matches
    in  case'

to'core (AST.Hole name)
  = error $ "To'Core transformation: found hole '" ++ name ++ "'"

to'core (AST.Placeholder ph)
  = error $ "To'Core transformation: found placeholder = " ++ show ph 


section'to'core :: Bind'Section -> [Binding]
section'to'core (explicits, implicitss) =
  let expl'core = map (\ (Explicit _ bg) -> bind'group'to'core bg) explicits
      impl'core = concatMap (map (\ (Implicit bg) -> bind'group'to'core bg)) implicitss
      all'core = expl'core ++ impl'core
  in  all'core


data'to'core :: [AST.Data] -> [Binding]
data'to'core data'decls =
  let data'constrs = concatMap make'd'constrs data'decls
      
      make'd'constrs :: AST.Data -> [Binding]
      make'd'constrs AST.Data{ AST.constructors = constructors }
        -- TODO: for each constructor I need to create a Binding
        = map make'constr constructors

      -- Constr'Decl = Con'Decl Name [Type]
      make'constr :: AST.Constr'Decl -> Binding
      make'constr (AST.Con'Record'Decl tag fields)
        = let names = take (length fields) letters
              vars = map Var names
              body = Intro tag vars
              lambda = foldr Abs body names
          in Binding tag lambda

      make'constr (AST.Con'Decl tag arg'types)
        = let names = take (length arg'types) letters
        -- now I need to fold that into the lambda
              vars = map Var names
              body = Intro tag vars
              lambda = foldr Abs body names
          in  Binding tag lambda
  in data'constrs


decls'to'core :: [AST.Declaration] -> [Binding]
decls'to'core declarations = mapMaybe collect declarations


collect :: AST.Declaration -> Maybe Binding
collect (AST.Binding bg@AST.Bind'Group{}) = Just $ bind'group'to'core bg
collect _ = Nothing


match'to'core :: AST.Match -> Match
match'to'core m@AST.Match{ AST.patterns = pats, AST.rhs = expr } = Match{ patterns = pats, rhs = to'core expr }


bind'group'to'core :: AST.Bind'Group -> Binding
bind'group'to'core AST.Bind'Group{ AST.name = name, AST.alternatives = [AST.Match{ AST.patterns = [], AST.rhs = rhs }] }
  -- NOTE: If there are no patterns, and only single equation, it is a variable declaration
  --      I then only produce a Binding, for the name and the right hand side
  = Binding{ name = name, lambda = to'core rhs }

bind'group'to'core AST.Bind'Group{ AST.name = name, AST.alternatives = matches }
  = Binding{ name = name, lambda = lambda }
    -- TODO: strategy for translating a full bindgroup into a lambda
    -- example:
    -- foo <P A 1> <P A 2> = <E A>
    -- foo <P B 1> <P B 2> = <E B>
    --
    -- this gets translated into this:
    -- foo = \ x -> \ y -> case x of
    --                       <P A 1> -> case y of
    --                                    <P A 2> -> <E A>
    --                                    _ -> case x of
    --                                            SPLITPOINT $1
    -- SPLITPOINT #1:
    --                       <P B 1> -> case y of 
    --                                    <P B 2> -> <E B>
    --                                    _ -> error# Non'Exhaustive
    --                       _ -> error# Non'Exhaustive
    --
    -- this is kinda involved and ineffient
    -- but I don't mind
    -- there is a lot of repetition


    -- So I think I am going to approach it like this:
    -- I can basically use the same infrastructure as for the lambda
    -- only difference is, that the top level expression, needs to 

    where lambda  = foldr Abs case' (first'var : vars)
          -- first I need to assign each pattern a unique variable name
          -- 
          (AST.Match{ AST.patterns = patterns } : _) = matches
          first'var : vars = take (length patterns) letters
          -- now I can use that each time a handle'match is called
          
          -- error boundary for the absolute bottom of the pattern matching
          failing'match = Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive }
          
          (_, matches') = mapAccumR handle'match [failing'match] matches

          -- now I compose all those matches into a large case and also add one error boundary
          case' = Case (Var first'var) (matches' ++ [failing'match]) -- I need to add failing'match because when accumulating-folding
          -- it is just an accumulator, it is never actually put inside the list of matches -- at its end


          handle'match :: [Match] -> AST.Match -> ([Match], Match) -- Case expression
          handle'match eqs'below AST.Match{ AST.patterns = first'pattern : patterns, AST.rhs = rhs }
            = let vars'patterns = zip vars patterns
                  rhs'  = to'core rhs

                  case' = build'case vars'patterns rhs'

                  build'case :: [(Name, Pattern)] -> Core -> Core
                  build'case pairs expr = foldr build' expr pairs
                    where
                      build' (n, p) e = let the'match       = Match{ patterns = [p], rhs = e }
                                            boundary'       = Match{ patterns = [P'Wild], rhs = Case (Var first'var) eqs'below {- [eqs'below, failing'match] -}}
                                        in  Case (Var n) [the'match, boundary']

                  complete'match = Match{ patterns = [first'pattern], rhs = case' }
                  -- new'below = Match{  }

              in (complete'match : eqs'below, complete'match)

          handle'match _ AST.Match{ AST.patterns = [] }
            = error "This should never happen #231"





    -- where lambda  = foldr Abs case' vars
    --       -- first I need to assign each pattern a unique variable name
    --       -- 
    --       (AST.Match{ AST.patterns = patterns } : _) = matches
    --       vars    = take (length patterns) letters
    --       -- now I can use that each time a handle'match is called

    --       matches' = map handle'match matches
    --       -- now a Map those matches' to just the first branch in each case
    --       -- since I don't allow top level patterns, this should be safe
    --       -- but I also don't allow local level - only patterns, so this kinda sucks
    --       -- 
    --       just'first'ones = map (\ (Case _ (m: ms)) -> m) matches'
    --       -- now I compose all those matches into a large case and also add one error boundary
    --       (first'var : _) = vars
    --       case' = Case (Var first'var) (just'first'ones ++ [Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive}])


    --       handle'match :: AST.Match -> Core -- Case expression
    --       handle'match AST.Match{ AST.patterns = patterns, AST.rhs = rhs }
    --         = let vars'patterns = zip vars patterns
    --               -- I think I can now basicaly do the same thing as above
    --               -- a build a case expression

    --               rhs'  = to'core rhs

    --               case' = build'case vars'patterns rhs'

    --               build'case :: [(Name, Pattern)] -> Core -> Core
    --               build'case pairs expr = foldr build' expr pairs
    --                 where
    --                   build' (n, p) e = let the'match       = Match{ patterns = [p], rhs = e }
    --                                         error'boundary  = Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive }
    --                                     in  Case (Var n) [the'match] -- , error'boundary]

    --           in  case'


is'prim'op :: Name -> Bool
is'prim'op name = elem name prim'ops
  where prim'ops =  [ "int#=="
                    , "int#<"
                    , "int#>"
                    , "int#+"
                    , "int#show"
                    , "double#+"
                    , "int#*"
                    , "double#*"
                    , "int#-"
                    , "double#-"
                    , "double#/"
                    , "int#/"
                    , "trace#" ]
