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