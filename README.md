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
    each alias will reflect the stage of the collecting information I am currently at.
    But more imporantly - they will all share the Counter in the StateT portion of the stack
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
- I also need to check that they (class declarations and instance declarations) are included in all the important analyses so far
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
