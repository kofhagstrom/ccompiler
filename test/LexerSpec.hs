module LexerSpec (spec) where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "" $
    it "" $
      lexChars (lines "a = 1 ? 2 : 3; if (flag) return 0; else if (other_flag) return 1; else return 2;")
        `shouldBe` Right
          [ LiteralT (IdentifierL "a"),
            AssignmentT,
            LiteralT (IntL 1),
            QuestionMarkT,
            LiteralT (IntL 2),
            ColonT,
            LiteralT (IntL 3),
            SemiColonT,
            KeywordT IfKW,
            OpenParenthesisT,
            LiteralT (IdentifierL "flag"),
            CloseParenthesisT,
            KeywordT ReturnKW,
            LiteralT (IntL 0),
            SemiColonT,
            KeywordT ElseKW,
            KeywordT IfKW,
            OpenParenthesisT,
            LiteralT (IdentifierL "other_flag"),
            CloseParenthesisT,
            KeywordT ReturnKW,
            LiteralT (IntL 1),
            SemiColonT,
            KeywordT ElseKW,
            KeywordT ReturnKW,
            LiteralT (IntL 2),
            SemiColonT
          ]
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
