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


to'core :: AST.Expression -> Core
to'core (AST.Var name)
  = Var name

to'core (AST.Const name)
  = Const name

to'core (AST.Op name)
  = Op name

to'core (AST.Lit lit)
  = Lit lit

to'core (AST.Abs pattern expr)
  = let (patterns, body)  = decompose expr
        body'             = to'core body
        all'patterns      = pattern : patterns

        vars'patts        = zip (take (length all'patterns) letters) all'patterns
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

to'core (AST.Placeholder _)
  = error "To'Core transformation: found placeholder"


decls'to'core :: [AST.Declaration] -> [Binding]
decls'to'core declarations = mapMaybe collect declarations


collect :: AST.Declaration -> Maybe Binding
collect (AST.Binding bg@AST.Bind'Group{}) = Just $ bind'group'to'core bg
collect _ = Nothing


match'to'core :: AST.Match -> Match
match'to'core m@AST.Match{ AST.patterns = pats, AST.rhs = expr } = Match{ patterns = pats, rhs = to'core expr }


bind'group'to'core :: AST.Bind'Group -> Binding
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
    --                                    _ -> error# Non'Exhaustive
    --                       <P B 1> -> case y of 
    --                                    <P B 2> -> <E B>
    --                                    _ -> error# Non'Exhaustive
    --                       _ -> error# Non'Exhaustive
    --
    -- So I think I am going to approach it like this:
    -- I can basically use the same infrastructure as for the lambda
    -- only difference is, that the top level expression, needs to 
    
    where lambda  = foldr Abs case' vars
          -- first I need to assign each pattern a unique variable name
          -- 
          (AST.Match{ AST.patterns = patterns } : _) = matches
          vars    = take (length patterns) letters
          -- now I can use that each time a handle'match is called

          matches' = map handle'match matches
          -- now a Map those matches' to just the first branch in each case
          -- since I don't allow top level patterns, this should be safe
          -- but I also don't allow local level - only patterns, so this kinda sucks
          -- 
          just'first'ones = map (\ (Case _ (m: ms)) -> m) matches'
          -- now I compose all those matches into a large case and also add one error boundary
          (first'var : _) = vars
          case' = Case (Var first'var) (just'first'ones ++ [Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive}])


          handle'match :: AST.Match -> Core -- Case expression
          handle'match AST.Match{ AST.patterns = patterns, AST.rhs = rhs }
            = let vars'patterns = zip vars patterns
                  -- I think I can now basicaly do the same thing as above
                  -- a build a case expression

                  rhs'  = to'core rhs

                  case' = build'case vars'patterns rhs'

                  build'case :: [(Name, Pattern)] -> Core -> Core
                  build'case pairs expr = foldr build' expr pairs
                    where
                      build' (n, p) e = let the'match       = Match{ patterns = [p], rhs = e }
                                            error'boundary  = Match{ patterns = [P'Wild], rhs = Error Non'Exhaustive }
                                        in  Case (Var n) [the'match, error'boundary]

              in  case'
      --map match'to'core matches
-- TODO: this function needs to build the single lambda from the whole bind group