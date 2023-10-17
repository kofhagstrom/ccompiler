module LexerSpec (spec) where

import Lexer
import Test.Hspec

spec :: Spec
spec = do
  describe "" $
    it "" $
      lexChars
        ( lines $
            "int main(){"
              ++ "a = 1 ? 2 : 3;"
              ++ "if (flag) return 0;"
              ++ "else if (other_flag) return 1;"
              ++ "else return 2;"
              ++ "a==!~-(b+c);"
              ++ "a>b;"
              ++ "b<a;"
              ++ "a>=b;"
              ++ "a<=b;"
              ++ "a!=b;"
              ++ "a&&b;"
              ++ "a||b;"
              ++ "a=b;"
              ++ "}"
        )
        `shouldBe` Right
          [ KeywordT IntKW,
            LiteralT (IdentifierL "main"),
            OpenParenthesisT,
            CloseParenthesisT,
            OpenBraceT,
            LiteralT (IdentifierL "a"),
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
            SemiColonT,
            LiteralT (IdentifierL "a"),
            LogicalEqualityT,
            BangT,
            TildeT,
            MinusT,
            OpenParenthesisT,
            LiteralT (IdentifierL "b"),
            PlusT,
            LiteralT (IdentifierL "c"),
            CloseParenthesisT,
            SemiColonT,
            LiteralT (IdentifierL "a"),
            GreaterThanT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            LiteralT (IdentifierL "b"),
            LessThanT,
            LiteralT (IdentifierL "a"),
            SemiColonT,
            LiteralT (IdentifierL "a"),
            GreaterThanOrEqualT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            LiteralT (IdentifierL "a"),
            LessThanOrEqualT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            LiteralT (IdentifierL "a"),
            NotEqualT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            LiteralT (IdentifierL "a"),
            AndT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            LiteralT (IdentifierL "a"),
            OrT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            LiteralT (IdentifierL "a"),
            AssignmentT,
            LiteralT (IdentifierL "b"),
            SemiColonT,
            CloseBraceT
          ]
