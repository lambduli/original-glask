# Glask


# TODO:
- ## Lexer
- ## Parsing Stage 1
  - resolve all shift/reduce conflicts (10)
- ## Parsing Stage 2
  - construct application trees from application lists (Extended Shunting Yard Algorithm)

- ## Building the Trans'Env
  - in the process of building the Translate Environment I have run into the problem
    to assign each type constructor a freshly created Kind I need to run the analysis
    in the context of the State monad
    BUT I don't have the whole Translate Environment built yet, so I don't have a Translate monad
  - I think I have found a solution:
    I will create multiple type aliases based on the same monad transformer stack
    each alias will reflect the stage of the collecting information I am currently at
    but more imporantly - they will all share the Counter in the StateT portion of the stack
    on the top level - the function which handles this part of the process
    will run the analysis or multiple in the correct context and each time it produces a result it will also produce a state
    that state will be used to initialize the next context
  - this design will introduce some overhead, but maybe it will make it work just right

- ## Semantic Analysis and Transformation
  - merge all binding groups (at first each one only contains single equation) into a single group
  - check that all data constructors which are operators and defined as POST/PRE-fix are always unary (consider having the similar requirement on binary infix operators)
  - check type contexts validity (read Haskell Report for that and consult my Parser.y - specificaly Class\Instance declarations)
    - basic structure checking is done by the parser - add better error reporting in the future
  - type synonym substitution
    But I think it would be nice to collect kind constraints from non expanded synonyms and type annotations mentioning them for better error reporting maybe?
    How to make sure that the error mentioning the synonym is found and reported before the error from the expanded type?
    I don't think that is necessary now.

- ## Error Reporting
  - propagate locations through the representations
  - figure out how to tie locations with type inference

- ## Serialization / Pretty Printing
  - I am currently the most inclined to the idea of using the original input as user wrote it and just decorating it with informations and errors - all of that have to be made possible by location informations
  - implement show (for debugging) for Match'Group and Match

- ## Kind Analysis
  - Inference itself is implemented [+]
  - Implement the wiring parts
  - Write tests

- ## Type Analysis
  - Most of the inference / type analysis is implemented
  - I still need to implement the inference for the top level stuff I think? - that should be my current priority
  - Write tests

- # Notes on the implementation
  - ? make the Kind Variables in the Type Variables correct (this will be done by the part of implementation which translates from Term'Type to Type)
      - it's going to work like this:
        - what is the only way for a type variable to be introduced?
        - it's the type annotation (also, there are no "local types" in types)
        - first time the variable is introduced is the same as the last time it's mentioned
          - this doesn't really break something like `ScopedTypeVariable`, because it can work like this:
            - when translating an annotated declaration, I will first translate the type annotation
            - that will produce a type, I can then look at the type and register all the type variables in the kind context and within such context I can translate the expression part of the definition
            - that will lead to the type variables all being correctly registered already, so that later, when such type variable is approached within the body, it is assigned the same kind variable as other occurences -> that makes them equal
          - but that also means, that when dealing with local declarations inside a global declarations - the local declaration, which could register it's own variables, must not override any assignments already in the context, because that would break the association between the top level declaration's type variable and the local one
            - so before registering a new binding in the kind context, one must make sure that the corresponding binding is not already present in there -> if it happens to be already present, it will be used as is and not overwritten
        - but that means, that each time a type annotation is processed, that level must do some bookkeeping:
          - first identify all free type variables (that would pretty much be all type variables present)
          - then remove from those, all those which are already registered in the kind context
            (those are already taken care of)
          - and only register what is left
      - !!! If I want to make a `ScopedTypeVar` work, I need to take care of the type annotation type inference rule
        - because that means, that those type variables from the surrounding scope are not going to be closed over
        - that will definitely change the rule for infering annotated expression
        - maybe it will work just fine when I will conditionally NOT close over that specific (already closed over) type variable, similarly how it works just fine for type variables from outer scope (leading to deffered predicates) - I will need to check this thoroughly


# Next Steps:
- Transform the `Term` to the `AST` (I think the Kind Variables in the Types are handled now)
- Keep adding tests for the Lexer and the Parser
- Change the Parser State monad to something possibly also based on the ExceptT -> so I can report syntactic and semantic errors better
- Prepare the implementation of the Shunting Yard Algorithm
- Desugar list and tuple patterns
- Figure out the tuple constructors
- Other possible stuff

- ## Analyses:
  - Dependency Analysis will need to be perfored for every `let ... = ... in ...` expression --> I will isolate it into the function and return the result in some reasonable shape to be used for the specific thing
  - Dependency Analysis for Types
  - Dependency Analysis
  - Dependency Analysis for Type Class declarations (no cyclic dependencies)


# Questions:

## 0

Why can't I have type classes defined only for higher kinded types?
ex.:
```haskell
class Wrapper (a b) where
  value :: a b -> b

instance Wrapper (Maybe a) where
  value (Just a) = a
  value _ = error
```
What's wrong with that?
It is still one type, only there I have multiple variables.

> I should definitely investigate!

## 1

```haskell
data NotEither a b = Up a | Down b

class Example a


instance Example (Either a b) -- and not instance Example (Either Int Char)

-- but not
-- foo :: Example (a b c) => ...
-- this is WRONG - the context above is valid
-- the confusion probably came from this annotation
-- foo :: Example (a b c) => a String String -> a String String
-- then `b` and `c` are ambiguous variables and are preventing the constraint from being solved
foo :: Example (a Int Char) => a String String -> a String String
foo x = x

x = foo $ Right "hello"

y = foo $ Up "hello" -- this one fails to type check
```

I think I understand the `instance` restriction.
It makes some sense to force higher-kinded types to be perfectly polymorphic instances.

But then WHY do I have to apply the type variable to at least one concrete type?
It's not like there is a way to declare the specific instance for that.
So it would pretty much could as well be Example (a b c).

I have a feeling this would cause some troubles though. Variables `b` and `c` would then be ambiguous.
So that's probably the reason.

And it's not like you can do something like:

```
instance Example Either
```

Because it's kind is `* -> * -> *`. So that's probably that.


## 2

```haskell
data NotEither a b = Up a | Down b

class Example a


instance Example (Either a b)

--              a = (Either x) I think
foo :: Example (a (Maybe b)) => a b -> a b
foo x = x

x = foo $ Right "hello"
```

My question is. Can we safely assume that b is pretty much unrestricted?
Because from the fact that instances can be only defined in the way they can be defined.
We can infer that (Maybe b) can pretty much be any type (the `b` in the `Example` instance declaration for `Either`). So from that, any part of the pretty much unrestricted type can be pretty much unrestricted.
So the `b` from the `foo`'s annotation can be anything. And I think the context could be simplified.
`Example (a _ _) => a b -> a b`

Where `_` must be some concrete type. 


## 3

Is there any way, to qualify over type constructors?
ex.:

```haskell
data Dictionary k v = ...
```
Now, what if I would like to say, that whatever the `k` is, it must have an instance of `Ord`?

I think the workaround would be to have "smart constructors" and they would qualify the type, but that is not that ergonomic.

## Answer (3)

No.
I can use GADTs and they will bring the context in some cases (let behaves strangely) but aside from that - no.


## Q 4

Why does `ghc` only report warning on missing field initialization?
Shouldn't that be pretty much an error? Because partially applying the constructor would still produce a resulting value of a different type, so why not with record syntax?

## Answer (4)

Because it initializes them to `undefined` by default.

## Idea:
Maybe I could refactor the Terms so that they are parametrized by the type they represent.
So `Term'Type` would become `Term Type` and so on.

I think the only benefit would be to have the connection between them explicitly specified.
But then again - I am explicitly specifying that connection when defining the instances of `ToAST`.


## Idea:
Could I remove the `Intro` constructor from the Expression and instead go like this:

  - do the inference on the `AST` without generated code for evaluation
  - only then do some code generation into some `IR` which will contain stuff taking care of actually constructing/introducing a value into the "evaluation state"

I think that would make a lot of sense and make things easier to understand.
I would infer the type of the constructor easily - it is just a constant registered in the typing context.
Sound good - ask Ryan for a technical term describing the act of actually generating/constructing an actual runtime representation of the value using the constructor and its arguments.


## Idea / Question:
Would it be desirable to have the option to define parts of the binding group throughout the whole program?
Basicaly having the binding group being open - in the same sense as type classes are open.

That would allow programmer to declare those bindings even in different modules.



## Interesting Note:



```haskell
{-# LANGUAGE ExplicitForAll, ScopedTypeVariables, NoMonomorphismRestriction #-}

class Pred a

class Aft a

-- foo :: forall a b . (Pred a, Aft a) => a -> b -> a
-- foo x y
--   = bar (x :: a) :: Pred a => a
-- this works fine
-- it also shows that you CAN only specify some of the constraints
-- in the "inline" type annotation if you take care
-- of the rest of them in the surrounding type annotation

foo
  = (\ x y -> bar x :: Pred a => a)
    :: forall a b . Pred a => a -> b -> a
-- doesn't work
-- because the (Aft a1) can not be deduced from neither of these:
--     the inferred type for foo (interesting!!!)
--     from the type annotation of the whole function
--     from the "inline" type annotation


-- foo
--   = (\ x y -> bar x :: Pred a => a )
--     :: forall a b . (Pred a, Aft a) => a -> b -> a
-- this works with NoMonomorphismRestriction enabled

-- foo = (\ x y -> bar x) -- doesn't work without NoMonomorphismRestriction
-- foo x y = bar x -- works even without NoMonomorphismRestriction

bar :: forall a . (Pred a, Aft a) => a -> a
bar a = a :: a

main = putStrLn "Hello, World!"
```


## Interesting Note:

Regarding the page 35/38 in THIH.

> Note also that if _es_ is empty, then _n_ must be 1.

`I don't think this is true in current version of Haskell. I also don't believe it is necessary.`

```haskell
my'foldl acc _ [] = acc
my'foldl acc fn (i : is)
  = my'foldl (fn acc i) fn is
  -- where _ = foo undefined

foo x = my'foldl 0 (+) [1 :: Int,2,3]
```

Contrary to what THIH mentions - it seems like even thought there are no explicitly typed bind groups,
the implicitly typed ones are split to (at least) two groups.
Only if I uncomment the fourth line, it is forced to typecheck them in the same group and it leads to forced monomorphization of the `my'foldl` function.


## Discussion:

Page 32/38 of THIH.
Code snippet.

line where `fs = tv (apply s as)`.

Questions is - why do I need to apply the substitution to the typing context from the beggining of this inference?

My understanding - `as` can only contain the assumptions about "global" variables (from the perspective of this explicitly annotated biding).
It also contains inferred assumptions from all previous implcits.
And finally it contains assumptions for all the explicits. So even this specific one.

Now - when inferring the "alts" for the explicit, I think I will somehow interact with this typing context.
Like creating some type cosntraints mentioning stuff from the typing context. (That seems possible.)
But WHY would I want to substitute/update the typing context after that?

Maybe some of those typing constraints will be able to specify something from the context?
I honestly don't think so - that would mean, that some later inference round (for later binding group) would be able to
change the type of the things from the previous groups. That can't happen!

So applying a substitution obtained by solving constraints from one explicit - to a typing context full of
assumptions about bindings from preceding binding groups makes no sense to me.


So maybe let's try something different.
In the code the following line is this:
`gs = tv t' \\ fs`
Nowhere else is the `fs` variable used.
So what if it is really applied only for the `tv` function application?

Well, free type variables in the typing context are looked up like this:
It just looks for all free variables in the Scheme part of the typing context/assumptions.
Finding free variables in the Scheme looks for all free type variables in the qualified type and removes those, which are mentioned in the quantifier part.
Free variables of the qualified type is defined as an union of free variables in the qualifier part and free variables in the type part.
Predicate part is simple - all variables are going to be free. They are not bound by any type scheme - that's why.
The type part is going to be simple too. All type variables are considered free - the type scheme will remove those, which are in fact bound.

So the question would be:
Could it happen, that application of the substitution to the typing context, would change the result of the `free'vars`?
It definitely could. If the substitution would replace some free type variable with some type, which would contain different free variables
than what was the "begin-replaced" variable.
Than that would lead to possibly registering some new free variables.

BUT - can that even happen? Is it possible to run inference on the `[Alt]` and obtain such constraints, that when solved, it will produce
a substitution which will add some more specificity to the types already present in the assumptions?
Those are just assumptions about top level declarations right?
Not even local assumptions make it into the typing context - they are passed down locally, but never make it onto the higher level.
Because that would cause leaking of locally declared variables and their types.

So WHY should I apply the substitution to the typing context?


As I am looking right now - I might actually be doing it too in the Frea. I will need to investigate those two lines:

```haskell
(t'env', t'constrs', k'constrs') <- put'in't'env (bind'name, closeOver $ apply subst bind'type) (infer'groups sccs)
```

and 

```haskell
(t'env', t'constrs', k'constrs') <- merge'into't'env (map (second (closeOver . apply subst)) t'binds) (infer'groups sccs)
```

I need to find out if it is possible, that those functions `put'in't'env` and/or `merge'into't'env` could possibly modify the typing context
in any way - aside from updating/inserting those explicitly mentioned assumptions.

Answer: I don't think that any of that is happening.
What I do is - for each binding in the SCC component - I must apply the substitution to the infered type for that binding,
than I put the result into the typing context - because so far it was there only registered as a *fresh and free type variable*.
This way I make it the thing I figured it should be and put it into the typing context for the following rounds of inference for
following (dependent) SCC components.

That is - because in Frea I just assigned each binding (annotated or not) a *free and fresh type variable* which later needs to be
substituted - immidiately when the constraints are solved.

So my conclusion - I just don't know why I would need to do it.


> In any case, I still can do it the way it is done in the THIH, using a combination of `ask` and `local`



## Question:

Maybe I should merge the `Infer` and `Solve` monads into a one - then I would be able to interleave the unification and inference.
(implementation of `infer'expl` for instance is bit awkward because of them being two things)


## Inference for class members

I think it's correct to treat the class members as `explicits` even though they will share the same name with multiple other definitions.
That should not be a problem - it will be slightly inefficient when putting those assumptions into the Typing Context,
but I can fix that later.
That also means that I don't need to compute dependency analysis on instance member declarations.


## Discussion about Monomorphism Restriction

```haskell
five = 2 + 3
```

with monomorphism restriction enabled five has the type `Integer`

but with it disabled it has a type `Num a => a`.

So I think the defaulting substitution is what is happening and what makes some declarations possible even in the presence of the monomorphism restriction, because it is such a pain to have to work around it.
I wonder whether I will validate this hypothesis in the future.


## Discussion

```haskell
{-# LANGUAGE MultiParamTypeClasses #-}

class Operation a b where
  operation :: a -> b

instance Operation () Int where
  operation _ = 23

instance Operation Int Bool where
  operation _ = True


main = do
  let a = operation ()
  let b = operation a
  -- print a
  -- print b
  print "End."
```

What happens - type error for ambiguous type variable because of use of `operation`.
I would need to annotate the `a` with correct type. The question is - WHY?
At first sight - this is pretty obvious example isn't it?
Investigate it.
