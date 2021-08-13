import Test.Hspec

import qualified LexerSpec


main :: IO ()
main = hspec spec


spec :: Spec
spec = do
  describe "Testing Lexer" LexerSpec.spec
