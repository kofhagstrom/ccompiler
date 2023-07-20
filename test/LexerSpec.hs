module LexerSpec (spec) where

import Lexer
import Test.Hspec

spec :: Spec
spec =
  describe "" $
    it "" $
      lexChars (lines "int main(){return -!142;}")
        `shouldBe` Right
          [ KeywordT IntKW,
            LiteralT (IdentifierL "main"),
            OpenParenthesisT,
            CloseParenthesisT,
            OpenBraceT,
            KeywordT ReturnKW,
            MinusT,
            BangT,
            LiteralT (IntL 142),
            SemiColonT,
            CloseBraceT
          ]
