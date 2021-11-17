module ParserSpec where


import Test.Hspec


import Compiler.Syntax.Term


import Compiler.Syntax.Literal


import Compiler.Parser.Parser (parse'module, parse'decls, parse'expr, parse'type)


infix 5 ~:, ~.., ~::

(~:) :: String -> Term'Expr -> Bool
source ~: term = result == term
  where result = parse'expr source


(~::) :: String -> Term'Type -> Bool
source ~:: type' = result == type'
  where result = parse'type source


(~..) :: String -> [Term'Decl] -> Bool
source ~.. decl = result == decl
  where result = parse'decls source


spec :: Spec
spec = do
  describe "Test parsing of type annotations" $ do

    it "parses a simple type signature" $ do
      "foo :: Int" ~.. [Signature "foo" ([], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a simple type signature" $ do
      "foo :: a" ~.. [Signature "foo" ([], Term'T'Id $ Term'Id'Var "a")]

    it "parses a type signature with qualified type" $ do
      "foo :: Bar a => Int" ~.. [Signature "foo" ([Is'In "Bar" $ Term'T'Id $ Term'Id'Var "a"], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a type signature with qualified type" $ do
      "foo :: (Bar a) => Int" ~.. [Signature "foo" ([Is'In "Bar" $ Term'T'Id $ Term'Id'Var "a"], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a type signature with qualified type - bigger context" $ do
      "foo :: (Bar a, Baz b) => Int" ~.. 
        [ Signature"foo" ([ Is'In "Bar" $ Term'T'Id $ Term'Id'Var "a"
                          , Is'In "Baz" $ Term'T'Id $ Term'Id'Var "b"], Term'T'Id $ Term'Id'Const "Int")]

    it "parses a type signature with qualified type with empty context" $ do
      "foo :: () => Int" ~.. [Signature "foo" ([], Term'T'Id $ Term'Id'Const "Int")]


  describe "Test parsing of a simple variable declarations" $ do

    it "parses a simple variable binding" $ do
      "foo = 23" ~.. [Binding (Term'P'Id (Term'Id'Var "foo")) (Term'E'Lit (Lit'Int 23))]

    it "parses a simple variable binding" $ do
      "bar = []" ~.. [Binding (Term'P'Id (Term'Id'Var "bar")) (Term'E'Id (Term'Id'Const "[]"))]


  describe "Test parsing of a simple class declarations" $ do

    it "parses a small class declaration" $ do
      "class Foo a" ~.. [Class "Foo" "a" [] []]


  describe "Test parsing type expressions" $ do

    it "parses a type variable" $ do
      "t'var" ~:: Term'T'Id (Term'Id'Var "t'var")

    it "parses a type constant" $ do
      "Int" ~:: Term'T'Id (Term'Id'Const "Int")

    it "parses a type application `Maybe a`" $ do
      "Maybe a" ~:: Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Var "a")]

    it "parses a type application `Maybe Int`" $ do
      "Maybe Int" ~:: Term'T'App [Term'T'Id (Term'Id'Const "Maybe"), Term'T'Id (Term'Id'Const "Int")]

    it "parses a unit type `()`" $ do
      "()" ~:: Term'T'Id (Term'Id'Const "()")

    it "parses a small tuple `(Int, String, Maybe Int)`" $ do
      "(Int, String, Maybe Int)" ~:: Term'T'Tuple [ Term'T'Id (Term'Id'Const "Int")
                                                  , Term'T'Id (Term'Id'Const "String")
                                                  , Term'T'App  [ Term'T'Id (Term'Id'Const "Maybe")
                                                                , Term'T'Id (Term'Id'Const "Int") ]]

    it "parses a simple function type `Int -> Int`" $ do
      "Int -> Int" ~:: Term'T'Arrow [ Term'T'Id (Term'Id'Const "Int")
                                    , Term'T'Id (Term'Id'Const "Int")]

    it "parses a function type `Int -> a -> Maybe a`" $ do
      "Int -> a -> Maybe a" ~:: Term'T'Arrow  [ Term'T'Id (Term'Id'Const "Int")
                                              , Term'T'Arrow  [ Term'T'Id (Term'Id'Var "a")
                                                              , Term'T'App  [ Term'T'Id (Term'Id'Const "Maybe")
                                                                            , Term'T'Id (Term'Id'Var "a") ] ] ]

    it "parses a function type `(b -> a) -> Either a b -> Result a`" $ do
      "(b -> a) -> Either a b -> Result a" ~::
        Term'T'Arrow  [ Term'T'Arrow [Term'T'Id (Term'Id'Var "b"), Term'T'Id (Term'Id'Var "a")]
                      , Term'T'Arrow  [ Term'T'App  [ Term'T'Id (Term'Id'Const "Either")
                                                    , Term'T'Id (Term'Id'Var "a")
                                                    , Term'T'Id (Term'Id'Var "b") ]
                                      , Term'T'App  [ Term'T'Id (Term'Id'Const "Result")
                                                    , Term'T'Id (Term'Id'Var "a") ] ] ]


  describe "Test parsing expressions" $ do

    it "parses '3 + 5'" $ do
      "3 + 5" ~: Term'E'App [Term'E'Lit $ Lit'Int 3, Term'E'Op $ Term'Id'Var "+", Term'E'Lit $ Lit'Int 5 ]
