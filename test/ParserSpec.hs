module ParserSpec (spec) where

import Lexer
import Parser
import Test.Hspec

parseHelper :: String -> Program
parseHelper string =
  case lexChars $ lines string of
    Right tokens -> parseAST tokens
    Left err -> error err

spec :: Spec
spec = do
  describe "Week 1" $ do
    it "return_0" $
      parseHelper "int main(){return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (Constant 0)
              ]
          )
    it "return_2" $
      parseHelper "int main(){return 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (Constant 2)
              ]
          )
    it "multi_digit" $
      parseHelper "int main(){return 100;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (Constant 100)
              ]
          )
  describe "Week 2" $ do
    it "bitwise" $
      parseHelper "int main() {return ~12;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp BitwiseComplement (Constant 12))
              ]
          )
    it "bitwise_zero" $
      parseHelper "int main() {return ~0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp BitwiseComplement (Constant 0))
              ]
          )
    it "nested_ops" $
      parseHelper "int main() {return !-3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp LogicalNegation (UnOp Negation (Constant 3)))
              ]
          )
    it "nested_ops_2" $
      parseHelper "int main() {return -~0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp Negation (UnOp BitwiseComplement (Constant 0)))
              ]
          )

    it "not_five" $
      parseHelper "int main() {return !5;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp LogicalNegation (Constant 5))
              ]
          )
    it "not_zero" $
      parseHelper "int main() {return !0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp LogicalNegation (Constant 0))
              ]
          )
  describe "week 3" $ do
    it "add" $
      parseHelper "int main() {return 1+2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (BinOp Addition (Constant 1) (Constant 2))
              ]
          )
    it "associativity" $
      parseHelper "int main() {return 1-2-3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  ( BinOp
                      Subtraction
                      (BinOp Subtraction (Constant 1) (Constant 2))
                      (Constant 3)
                  )
              ]
          )
    it "associativity_2" $
      parseHelper "int main() {return 6 / 3 / 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  ( BinOp
                      Division
                      (BinOp Division (Constant 6) (Constant 3))
                      (Constant 2)
                  )
              ]
          )
    it "precedence" $
      parseHelper "int main() {return 2 + 3 * 4;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  ( BinOp
                      Addition
                      (Constant 2)
                      (BinOp Multiplication (Constant 3) (Constant 4))
                  )
              ]
          )
    it "unop_parens" $
      parseHelper "int main() {return ~(1 + 1);}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (UnOp BitwiseComplement (BinOp Addition (Constant 1) (Constant 1)))
              ]
          )
    it "unop_add" $
      parseHelper "int main() {return ~2 + 3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (BinOp Addition (UnOp BitwiseComplement (Constant 2)) (Constant 3))
              ]
          )
    it "sub_neg" $
      parseHelper "int main() {return 2- -1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  (BinOp Subtraction (Constant 2) (UnOp Negation (Constant 1)))
              ]
          )
    it "parens" $
      parseHelper "int main() {return 2 * (3 + 4);}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Return
                  ( BinOp
                      Multiplication
                      (Constant 2)
                      (BinOp Addition (Constant 3) (Constant 4))
                  )
              ]
          )
