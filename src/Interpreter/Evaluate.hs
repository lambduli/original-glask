module Interpreter.Evaluate where


import qualified Data.Map.Strict as Map
import Control.Monad.State ( State, get, put )
import Control.Monad.Extra ( concatMapM )


import Compiler.Syntax.Name ( Name )
import Compiler.Syntax.Pattern ( Pattern(..) )
import Compiler.Syntax.Literal ( Literal(..) )

import Interpreter.Address ( Address )
import Interpreter.Promise ( Promise(..) )
import Interpreter.Environment ( Environment )
import Interpreter.Store
import Interpreter.Value ( Value(..) )
import Interpreter.State ( Machine'State )
import Interpreter.Core ( Core(..), Binding(..), Match(..) )
import Interpreter.Error ( Evaluation'Error(..) )


data Pattern'Match'Result a
  = OK a
  | Match'Failed


eval :: Core -> Environment -> Machine'State Value
eval (Var name) env = do
  -- look up the promise in the env
  case env Map.!? name of
    Nothing -> return $! Left $! Unbound'Var name -- NOTE: should never ever happen
    Just promise -> do
      -- force the promise
      force promise

-- eval (Const name) env = do
--   -- look up the promise in the env
--   case env `Map.!?` name of
--     Nothing -> return $! Left $! Unbound'Var name -- NOTE: should never ever happen
--     Just promise -> do
--       -- force the promise
--       force promise

eval (Prim'Op name) env = do
  return $! Right $! Operator name

eval (Lit lit) env = do
  return $! Right $! Literal lit

eval (Abs name body) env = do
  return $! Right $! Closure name body env

eval (App fn arg) env = do
  -- I evaluate the fn
  -- I should get a closure back
  -- I evaluate the body of the closure after extending its environment
  -- but for that I need to create a suspension and put it into the store
  -- and take the address of the slot and put it into the Promise and store that in the environment (as I said earlier)
  -- evaluating the body should produce a value, so that is what I return
  r <- eval fn env
  case r of
    Left err -> return $ Left err
    Right (Closure param body env') -> do
      store <- get
      let size = Map.size store
      let next'addr = size
      let new'store = Map.insert next'addr (Left (arg, env)) store
      put new'store
      let new'env = Map.insert param (Promise next'addr) env'
      eval body new'env

    Right _ -> return $ Left $ Unexpected "Evaluation Error: while evaluating function expression I didn't get a Closure!" -- NOTE: should never really happen

eval (Tuple cores) env = do
  let tag = "(" ++ replicate (length cores) ',' ++ ")"
  eval (Intro tag cores) env

eval (Let bindings body) env = do
  -- the goal is to register all bindings into the environment (and store therefore)
  -- then evaluate the body within that extended environment
  -- it's done like this:
  --
  -- I first create an environment, which will hold ALL of the bindings
  -- this environment is important, because not only the body will be evaluated within it
  -- but also all of the bodies of the bindings
  store <- get
  let next'addr = Map.size store

  let update :: (Address, [(Address, Name, Core)], Environment) -> Binding -> (Address, [(Address, Name, Core)], Environment)
      update (next'addr, collection, env') Binding{ name = n, lambda = core }
        = let next'env = Map.insert n (Promise next'addr) env'
          in  (next'addr + 1, (next'addr, n, core) : collection, next'env)

  let (_, triples, new'env) = foldl update (next'addr, [], env) bindings
  -- now I have all the important stuff to build the store
  -- that means that those suspensions (Core, Env)
  -- will all contain the final environment
  -- it is simple from here
  -- I then just evaluate the body of the let within the correct store and environment

  -- so I need to upt every suspension into the store
  let mini'store = Map.fromList $! map (\(addr, _, core) -> (addr, Left (core, new'env))) triples
  let new'store = store `Map.union` mini'store
  put new'store

  eval body new'env

eval (Case motive matches) env = do
  r <- eval motive env
  case r of
    Left err -> return r
    Right val -> do
      r <- pattern'match val matches
      case r of
        Left err -> return (Left err)
        Right Match'Failed -> return (Left (Unexpected "Evaluation - Pattern Matching: case analysis has no boundary."))
        Right (OK (captures, rhs)) -> do
          -- now I must iterate the captures
          -- each capture might hold a promise or a value
          -- if its a promise, that's ok, but if it's a value, that value needs to be stored into the store and a promise (pointer) to it will replace the original value in the pair
          r <- sequence <$> mapM make'env captures
          case r of
            Left err -> return (Left err)
            Right mini'env -> do
              -- now I just need to merge the mini'env with the main env and evalaute the rhs within it
              let new'env = env `Map.union` Map.fromList mini'env
              eval rhs new'env

    where
      make'env :: (Name, Either Promise Value) -> Machine'State (Name, Promise)
      make'env (n, Left p) = return (Right (n, p))
      make'env (n, Right val) = do
        -- first store the value into the store
        store <- get
        let next'addr = Map.size store

        let new'store = Map.insert next'addr (Right val) store
        put new'store

        -- now return the new association
        return (Right (n, Promise next'addr))


      -- This function tries each match, it builds the environment during pattern matching, it returns the environment and a right hand side of successful match
      pattern'match :: Value -> [Match] -> Machine'State (Pattern'Match'Result ([(Name, Either Promise Value)], Core))
      pattern'match val [] =
        return $ Left (Unexpected "Evaluation Error: There is no error boundary in the case analysis.")
      pattern'match val (Match{ patterns = [pattern], rhs = rhs } : ms) = do
        r <- pattern'match' val pattern
        case r of
          Left err -> return (Left err)
          Right (OK captures) -> return (Right (OK (captures, rhs)))
          Right Match'Failed -> pattern'match val ms

      pattern'match val (Match{ patterns = patts } : ms) = do
        return (Left (Unexpected "Evaluation Error: Pattern Matching - match inside a case analysis contains zero or many patterns and not exactly one."))
      
      pattern'match' :: Value -> Pattern -> Machine'State (Pattern'Match'Result [(Name, Either Promise Value)])
      -- NOTE: This extra case is for as patterns
      pattern'match' val (P'As alias pattern) = do
        r <- pattern'match' val pattern
        case r of
          Left err -> return r
          Right (OK captures) -> return (Right (OK $! (alias, Right val) : captures))
          Right Match'Failed -> return (Right Match'Failed)
      
      pattern'match' val pattern = do
        case val of
          Literal lit -> pattern'match'lit lit pattern
          d@(Data _ _) -> pattern'match'data d pattern
          o@(Operator _) -> pattern'match'prim'op o pattern
          c@(Closure _ _ _) -> pattern'match'prim'op c pattern

      pattern'match'data :: Value -> Pattern -> Machine'State (Pattern'Match'Result [(Name, Either Promise Value)])
      pattern'match'data d@(Data _ _) (P'Var name)
        = return (Right (OK [(name, Right d)]))

      pattern'match'data (Data tag promises) (P'Con name patterns)
        | tag == name =
          let list = concat'map'match (uncurry pattern'match'promise) $ zip promises patterns
          -- I don't need those function to be inside a Pattern'Match'Result
          -- quite the opposite, I think there is a point in not making them stateful
          -- that way it is aparent on the type level, that they can not modify the store
          -- then it should make some things a little bit simpler
          -- I won't need to use concatMapM variant maybe
          -- I will just have a result type which is one of three variants
          -- OK - matches, here is the capture
          -- DIDN'T MATCH - let's try another match case
          -- FAILED - while forcing a value, that is a runtime error and the whole program needs to be terminated
          -- then I just need a function, which will do the mapping thing, but short circuites if some of the elements results in FAILED or DIDN'T MATCH
          -- in all cases - it returns the same data structure, either containing all the captures, or unsuccessful match, or an error
          in undefined
      pattern'match'data d@(Data _ _) p@(P'As _ _)
        = pattern'match' d p
      pattern'match'data d@(Data _ _) P'Wild
        = return (Right (OK []))
      pattern'match'data d@(Data _ _) (P'Ann pattern _)
        = pattern'match'data d pattern
      pattern'match'data _ _
        = return (Right Match'Failed)


      concat'map'match :: ((Promise, Pattern) -> Machine'State (Pattern'Match'Result [a])) -> [(Promise, Pattern)] -> Machine'State (Pattern'Match'Result [a])
      concat'map'match fn [] = return (Right (OK []))
      concat'map'match fn (p : ps) = do
        -- first I run the fn on p
        r <- fn p
        case r of
          Left err -> return (Left err) -- short circuiting
          Right Match'Failed -> return (Right Match'Failed)
          Right (OK captures) -> do
            -- now I recursively evaluate the rest of the pairs
            rr <- concat'map'match fn ps
            case rr of
              Left err -> return (Left err)
              Right Match'Failed -> return (Right Match'Failed)
              Right (OK captures'acc) -> do
                -- now I combine the two results and return it
                return (Right (OK (captures ++ captures'acc)))


      pattern'match'promise :: Promise -> Pattern -> Machine'State (Pattern'Match'Result [(Name, Either Promise Value)])
      pattern'match'promise p@(Promise _) (P'Var name)
        -- promise is not forced, it is simply associated with the variable name
        = return $! Right $! OK [(name, Left p)]
      
      pattern'match'promise prom@(Promise _) pattern@(P'Con _ _) = do
        -- I force the promise to get a value
        -- then I pattern match it with existing function
        r <- force prom
        case r of
          Left err -> return (Left err)
          -- it needs to terminate the program, instead of continuing with another match
          Right val -> do
            pattern'match' val pattern
      
      pattern'match'promise prom@(Promise _) pattern@(P'Lit _) = do
        -- I force the promise to get a value
        -- then I pattern match it with existing function
        r <- force prom
        case r of
          Left err -> return (Left err) -- the same issue as above
          Right val -> do
            pattern'match' val pattern
      
      pattern'match'promise prom@(Promise _) (P'As alias pattern) = do
        result <- pattern'match'promise prom pattern
        case result of
          Left err -> return (Left err)
          Right (OK captures) -> return (Right (OK ((alias, Left prom) : captures)))
          Right Match'Failed -> return (Right Match'Failed)
      
      pattern'match'promise prom@(Promise _) P'Wild = do
        -- don't need to force
        return (Right (OK []))
      
      pattern'match'promise prom@(Promise _) (P'Ann pattern _) = do
        pattern'match'promise prom pattern


      pattern'match'prim'op :: Value -> Pattern -> Machine'State (Pattern'Match'Result [(Name, Either Promise Value)])
      pattern'match'prim'op prim'op (P'Var name)
        = return (Right (OK [(name, Right prim'op)]))
      pattern'match'prim'op prim'op p@(P'As _ _)
        = pattern'match' prim'op p
      pattern'match'prim'op prim'op P'Wild
        = return (Right (OK []))
      pattern'match'prim'op prim'op (P'Ann pattern _)
        = pattern'match'prim'op prim'op pattern
      pattern'match'prim'op _ _
        = return $ Left (Unexpected "pattern match failed, this is not an error, just a lazy/bad design :D")


      pattern'match'lit :: Literal -> Pattern -> Machine'State (Pattern'Match'Result [(Name, Either Promise Value)])
      pattern'match'lit l@(Lit'Int _) (P'Var name) = do
        return (Right (OK [(name, Right (Literal l))]))

      -- double part
      pattern'match'lit (Lit'Double d) (P'Lit (Lit'Double c)) | d == c = do
        return (Right (OK []))

      -- int part
      pattern'match'lit (Lit'Int i) (P'Lit (Lit'Int e)) | i == e = do
        return (Right (OK []))

      -- char part
      pattern'match'lit (Lit'Char ch) (P'Lit (Lit'Char x)) | ch == x = do
        return (Right (OK []))
      
      pattern'match'lit l (P'As alias pattern) = do
        r <- pattern'match'lit l pattern
        case r of
          Left _ -> return r
          Right (OK captures) -> return $! Right $! OK ((alias, Right (Literal l)) : captures)
          Right Match'Failed -> return (Right Match'Failed)
      
      pattern'match'lit _ P'Wild
        = return (Right (OK []))
      
      pattern'match'lit l (P'Ann pattern _)
        = pattern'match'lit l pattern
      
      pattern'match'lit _ _
        = return (Right Match'Failed)


eval (Intro tag cores) env = do
  -- this evaluates into the data construction
  -- cores (arguments) are translated into a suspension
  -- so they need to be put into a store one after other

  -- I transform each core argument into a (Core, Env)
  -- that will go into a store
  -- and a list of addresses (Promises) will go into a data value
  let suspensions = map (\ c -> Left (c, env)) cores

  -- now a associate those suspensions with their address (for their future slots)
  store <- get
  let next'addr = Map.size store
  let addresses = take (length suspensions) [next'addr ..]

  let pairs = zip addresses suspensions

  -- now I need to put all of those into the store
  let mini'store = Map.fromList pairs
  let new'store = store `Map.union` mini'store
  put new'store

  -- now I just need to construct the data
  let promises = map Promise addresses

  return $! Right $! Data tag promises

eval (Error err) env = do
  -- I just raise an error
  return (Left err)


force :: Promise -> Machine'State Value
force (Promise addr) = do
  store <- get
  let slot = Map.lookup addr store
  case slot of
    Nothing -> return $ Left $ Unexpected "Evaluation Error: incorrect address used."
    Just v -> case v of
      Left (core, env) -> do
        val <- eval core env
        case val of
          Left err -> return $ Left err
          Right val -> do
            -- now I need to update the store
            let new'store = Map.insert addr (Right val) store
            put new'store
            return $ Right val

      Right val -> return $ Right val
