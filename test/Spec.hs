import Test.Hspec

import qualified LexerSpec
import qualified ParserSpec
import qualified ExamplesSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Testing Lexer" LexerSpec.spec
  describe "Testing Parser" ParserSpec.spec
  describe "Test Language Examples" ExamplesSpec.spec
