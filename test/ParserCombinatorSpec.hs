module ParserCombinatorSpec (spec) where

import Data.List (intercalate)
import LexerCombinator
import ParserCombinator
  ( BinaryOperator (..),
    Expression (..),
    FuncDeclaration (..),
    Parser (..),
    Program (..),
    Statement (..),
    UnaryOperator (..),
    parseProgram,
  )
import Test.Hspec

parseHelper :: String -> Program
parseHelper string = case runLexer lexFile string of
  Right (tokens, _) -> case (runParser parseProgram tokens) of
    Right (ast, _) -> ast
    Left (es, ts) -> error ((concat $ show <$> es) ++ "Remaining tokens: " ++ (intercalate ", " $ show <$> ts))
  Left (e, _) -> error ((concat $ show <$> e))

spec :: Spec
spec = do
  describe "week_1" $ do
    it "return_0" $
      parseHelper "int main(){return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (Constant 0)
              )
          )
    it "return_2" $
      parseHelper "int main(){return 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (Constant 2)
              )
          )
    it "multi_digit" $
      parseHelper "int main(){return 100;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (Constant 100)
              )
          )
  describe "week_2" $ do
    it "bitwise" $
      parseHelper "int main() {return ~12;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (UnOp BitwiseComplement (Constant 12))
              )
          )
    it "bitwise_zero" $
      parseHelper "int main() {return ~0;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (UnOp BitwiseComplement (Constant 0))
              )
          )
    it "nested_ops" $
      parseHelper "int main() {return !-3;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (UnOp LogicalNegation (UnOp Negation (Constant 3)))
              )
          )
    it "nested_ops_2" $
      parseHelper "int main() {return -~0;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (UnOp Negation (UnOp BitwiseComplement (Constant 0)))
              )
          )

    it "not_five" $
      parseHelper "int main() {return !5;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (UnOp LogicalNegation (Constant 5))
              )
          )
    it "not_zero" $
      parseHelper "int main() {return !0;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (UnOp LogicalNegation (Constant 0))
              )
          )
  describe "week_3" $ do
    it "add" $
      parseHelper "int main() {return 1+2;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (BinOp Addition (Constant 1) (Constant 2))
              )
          )
    it "associativity" $
      parseHelper "int main() {return 1-2-3;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  ( BinOp
                      Subtraction
                      (BinOp Subtraction (Constant 1) (Constant 2))
                      (Constant 3)
                  )
              )
          )
    it "associativity_2" $
      parseHelper "int main() {return 6 / 3 / 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  ( BinOp
                      Division
                      (BinOp Division (Constant 6) (Constant 3))
                      (Constant 2)
                  )
              )
          )
    it "precedence" $
      parseHelper "int main() {return 2 + 3 * 4;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  ( BinOp
                      Addition
                      (Constant 2)
                      (BinOp Multiplication (Constant 3) (Constant 4))
                  )
              )
          )
    it "unop_parens" $
      parseHelper "int main() {return ~(1 + 1);}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  ( UnOp
                      BitwiseComplement
                      (BinOp Addition (Constant 1) (Constant 1))
                  )
              )
          )
    it "unop_add" $
      parseHelper "int main() {return ~2 + 3;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  ( BinOp
                      Addition
                      (UnOp BitwiseComplement (Constant 2))
                      (Constant 3)
                  )
              )
          )
    it "sub_neg" $
      parseHelper "int main() {return 2- -1;}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  (BinOp Subtraction (Constant 2) (UnOp Negation (Constant 1)))
              )
          )
    it "parens" $
      parseHelper "int main() {return 2 * (3 + 4);}"
        `shouldBe` Program
          ( Fun
              "main"
              ( Return
                  ( BinOp
                      Multiplication
                      (Constant 2)
                      (BinOp Addition (Constant 3) (Constant 4))
                  )
              )
          )

-- describe "week_4" $ do
--   it "and_false" $
--     parseHelper "int main() {return 1 && 0;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( Return
--                     (BinOp LogicalAnd (Constant 1) (Constant 0))
--                 )
--             ]
--         )
--   it "and_true" $
--     parseHelper "int main() {return 1 && 1;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( Return
--                     (BinOp LogicalAnd (Constant 1) (Constant 1))
--                 )
--             ]
--         )
--   it "skip_on_failure_multi_short_circuit" $
--     parseHelper "int main() {int a = 0; a || (a = 3) || (a = 4); return a;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ Declare (Declaration "a" (Just (Constant 0))),
--               State
--                 ( Expression
--                     (BinOp LogicalOr (BinOp LogicalOr (Variable "a") (Assignment "a" (Constant 3))) (Assignment "a" (Constant 4)))
--                 ),
--               State (Return (Variable "a"))
--             ]
--         )
--   it "skip_on_failure_short_circuit_and" $
--     parseHelper "int main() {int a = 0;int b = 0;a && (b = 5);return b;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ Declare (Declaration "a" (Just (Constant 0))),
--               Declare (Declaration "b" (Just (Constant 0))),
--               State (Expression (BinOp LogicalAnd (Variable "a") (Assignment "b" (Constant 5)))),
--               State (Return (Variable "b"))
--             ]
--         )
--   it "skip_on_failure_short_circuit_or" $
--     parseHelper "int main() {int a = 1;int b = 0;a || (b = 5);return b;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ Declare (Declaration "a" (Just (Constant 1))),
--               Declare (Declaration "b" (Just (Constant 0))),
--               State (Expression (BinOp LogicalOr (Variable "a") (Assignment "b" (Constant 5)))),
--               State (Return (Variable "b"))
--             ]
--         )
--   it "lt_false" $
--     parseHelper "int main() {return 2 < 1;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( Return
--                     (BinOp LessThan (Constant 2) (Constant 1))
--                 )
--             ]
--         )
-- describe "week_6" $ do
--   it "if_1" $
--     parseHelper "int main() {if (flag) return 0; else if (other_flag) return 1; else return 2;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( If
--                     (Variable "flag")
--                     (Return (Constant 0))
--                     (Just (If (Variable "other_flag") (Return (Constant 1)) (Just (Return (Constant 2)))))
--                 )
--             ]
--         )
--   it "if_2" $
--     parseHelper "int main() {if (flag) return 0; else {if (other_flag) return 1; else return 2;}}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( If
--                     (Variable "flag")
--                     (Return (Constant 0))
--                     (Just (If (Variable "other_flag") (Return (Constant 1)) (Just (Return (Constant 2)))))
--                 )
--             ]
--         )
--   it "if_3" $
--     parseHelper "int main() {if (flag) return 0;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( If
--                     (Variable "flag")
--                     (Return (Constant 0))
--                     Nothing
--                 )
--             ]
--         )
--   it "if_4" $
--     parseHelper "int main() {if (flag) return 0; else if (other_flag) return 1;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( If
--                     (Variable "flag")
--                     (Return (Constant 0))
--                     (Just (If (Variable "other_flag") (Return (Constant 1)) Nothing))
--                 )
--             ]
--         )
--   it "if_5" $
--     parseHelper "int main() {a = 1 ? 2 : 3;}"
--       `shouldBe` Program
--         ( Fun
--             "main"
--             [ State
--                 ( Expression
--                     ( Assignment
--                         "a"
--                         ( ConditionalExpression
--                             (Constant 1)
--                             (Constant 2)
--                             (Constant 3)
--                         )
--                     )
--                 )
--             ]
--         )
