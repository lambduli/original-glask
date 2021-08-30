# Glask


# TODO:
- ## Lexer
- ## Parsing Stage 1
- ## Parsing Stage 2
  - construct application trees from application lists
  - transform general expressions to patterns
  - ? make the Kind Variables in the Type Variables correct
- ## Semantic Analysis
  - check that all types are fully applied
  - check type contexts validity (read Haskell Report for that)

- ## Error Reporting
  - propagate locations through the representations
  - figure out how to tie locations with type inference

- ## Serialization / Pretty Printing
  - I am currently the most inclined to the idea of using the original input as user wrote it and just decorating it with informations and errors - all of that have to be made possible by location informations
  - implement show (for debugging) for Match'Group and Match



# Questions:

## 1

```haskell
data NotEither a b = Up a | Down b

class Example a


instance Example (Either a b) -- and not instance Example (Either Int Char)

-- but not
-- foo :: Example (a b c) => ...
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




# Next Steps:
- Keep adding tests for the Lexer and the Parser
- Define the `Term->AST` monad transformer stack - based on ExceptT
- Change the Parser State monad to something possibly also based on the ExceptT -> so I can report syntactic and semantic errors better
- Prepare the implementation of the Shunting Yard Algorithm
- Desugar list and tuple patterns
- Figure out the tuple constructors
- Other possible stuff 
- Transform the `Term` to the `AST`
  - Take care of the Kinds in Type Variables (again consider removing them altagether)

- ## Analysis:
  - Dependency Analysis will need to be perfored for every `let ... = ... in ...` expression --> I will isolate it into the function and return the result in some reasonable shape to be used for the specific thing
  - Dependency Analysis for Types
  - Dependency Analysisi 
  - Dependency Analysis for Type Class declarations (no cyclic dependencies)

> Can I do all the analyses AFTER the `Term->AST` transformations?
> Or is there something I need to analyze before I go from Term to AST?

The answer seems to be no. I think I could first transform the stuff, while doing so, I will also collect some information, like specific tuple constructors which need to be put inside the typing context, kinding context, type constant context and so on.


## Idea:
Maybe I could refactor the Terms so that they are parametrized by the type they represent.
So `Term'Type` would become `Term Type` and so on.

I think the only benefit would be to have the connection between them explicitly specified.
But then again - I am explicitly specifying that connection when defining the instances of `ToAST`.