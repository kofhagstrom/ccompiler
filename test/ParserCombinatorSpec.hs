module ParserCombinatorSpec (spec) where

import Data.List (intercalate)
import LexerCombinator
import ParserCombinator
import Test.Hspec

parseHelper :: String -> Program
parseHelper string = case run lexFile string of
  Right (tokens, _) -> case run parseProgram tokens of
    Right (ast, _) -> ast
    Left (es, ts) -> error (except es ++ "Remaining tokens: " ++ intercalate ", " (show <$> ts))
  Left (e, _) -> error (except e)

except :: (Show a) => [a] -> String
except = concatMap show

spec :: Spec
spec = do
  describe "misc" $ do
    it "newline" $
      parseHelper "int main() {\n return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 0)
                  )
              ]
          )
    it "comment" $
      parseHelper "int main() { // comment \n return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 0)
                  )
              ]
          )
  describe "week_1" $ do
    it "return_0" $
      parseHelper "int main(){return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 0)
                  )
              ]
          )
    it "return_2" $
      parseHelper "int main(){return 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 2)
                  )
              ]
          )
    it "multi_digit" $
      parseHelper "int main(){return 100;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 100)
                  )
              ]
          )
    it "no_newlines" $
      parseHelper "int main(){return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 0)
                  )
              ]
          )
    it "spaces" $
      parseHelper "   int   main    (  )  {   return  0 ; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (Constant 0)
                  )
              ]
          )
  describe "week_2" $ do
    it "bitwise" $
      parseHelper "int main() {return ~12;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator BitwiseComplement (Constant 12))
                  )
              ]
          )
    it "bitwise_zero" $
      parseHelper "int main() {return ~0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator BitwiseComplement (Constant 0))
                  )
              ]
          )
    it "nested_ops" $
      parseHelper "int main() {return !-3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator LogicalNegation (UnaryOperator Negation (Constant 3)))
                  )
              ]
          )
    it "nested_ops_2" $
      parseHelper "int main() {return -~0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator Negation (UnaryOperator BitwiseComplement (Constant 0)))
                  )
              ]
          )

    it "not_five" $
      parseHelper "int main() {return !5;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator LogicalNegation (Constant 5))
                  )
              ]
          )
    it "not_zero" $
      parseHelper "int main() {return !0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator LogicalNegation (Constant 0))
                  )
              ]
          )
    it "neg" $
      parseHelper "int main() { return -5;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (UnaryOperator Negation (Constant 5))
                  )
              ]
          )
  describe "week_3" $ do
    it "add" $
      parseHelper "int main() {return 1+2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Addition (Constant 1) (Constant 2))
                  )
              ]
          )
    it "associativity" $
      parseHelper "int main() {return 1-2-3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          Subtraction
                          (BinaryOperator Subtraction (Constant 1) (Constant 2))
                          (Constant 3)
                      )
                  )
              ]
          )
    it "associativity_2" $
      parseHelper "int main() {return 6 / 3 / 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          Division
                          (BinaryOperator Division (Constant 6) (Constant 3))
                          (Constant 2)
                      )
                  )
              ]
          )
    it "div" $
      parseHelper "int main() { return 4 / 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Division (Constant 4) (Constant 2))
                  )
              ]
          )
    it "div_neg" $
      parseHelper "int main() {return (-12) / 5;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Division (UnaryOperator Negation (Constant 12)) (Constant 5))
                  )
              ]
          )
    it "mult" $
      parseHelper "int main() { return 2 * 3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Multiplication (Constant 2) (Constant 3))
                  )
              ]
          )
    it "parens" $
      parseHelper "int main() {return 2 * (3 + 4);}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          Multiplication
                          (Constant 2)
                          (BinaryOperator Addition (Constant 3) (Constant 4))
                      )
                  )
              ]
          )
    it "precedence" $
      parseHelper "int main() {return 2 + 3 * 4;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          Addition
                          (Constant 2)
                          (BinaryOperator Multiplication (Constant 3) (Constant 4))
                      )
                  )
              ]
          )
    it "sub" $
      parseHelper "int main() { return 2- -1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Subtraction (Constant 2) (UnaryOperator Negation (Constant 1)))
                  )
              ]
          )
    it "sub_neg" $
      parseHelper "int main() {return 2- -1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Subtraction (Constant 2) (UnaryOperator Negation (Constant 1)))
                  )
              ]
          )
    it "unop_add" $
      parseHelper "int main() {return ~2 + 3;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          Addition
                          (UnaryOperator BitwiseComplement (Constant 2))
                          (Constant 3)
                      )
                  )
              ]
          )
    it "unop_parens" $
      parseHelper "int main() {return ~(1 + 1);}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( UnaryOperator
                          BitwiseComplement
                          (BinaryOperator Addition (Constant 1) (Constant 1))
                      )
                  )
              ]
          )
  describe "week_4" $ do
    it "and_false" $
      parseHelper "int main() {return 1 && 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LogicalAnd (Constant 1) (Constant 0))
                  )
              ]
          )
    it "and_true" $
      parseHelper "int main() {return 1 && 1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LogicalAnd (Constant 1) (Constant 1))
                  )
              ]
          )
    it "eq_false" $
      parseHelper "int main() { return 1 == 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Equality (Constant 1) (Constant 2))
                  )
              ]
          )
    it "eq_false" $
      parseHelper "int main() { return 1 == 1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Equality (Constant 1) (Constant 1))
                  )
              ]
          )
    it "eq_true" $
      parseHelper "int main() { return 1 == 1; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Equality (Constant 1) (Constant 1))
                  )
              ]
          )
    it "lt_false" $
      parseHelper "int main() {return 2 < 1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LessThan (Constant 2) (Constant 1))
                  )
              ]
          )
    it "ge_false" $
      parseHelper "int main() { return 1 >= 2; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator GreaterThanOrEqual (Constant 1) (Constant 2))
                  )
              ]
          )
    it "ge_true" $
      parseHelper "int main() { return 1 >= 1; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator GreaterThanOrEqual (Constant 1) (Constant 1))
                  )
              ]
          )
    it "gt_false" $
      parseHelper "int main() { return 1 > 2; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator GreaterThan (Constant 1) (Constant 2))
                  )
              ]
          )
    it "gt_true" $
      parseHelper "int main() {return 1 > 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator GreaterThan (Constant 1) (Constant 0))
                  )
              ]
          )
    it "le_false" $
      parseHelper "int main() {return 1 <= -1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LessThanOrEqual (Constant 1) (UnaryOperator Negation (Constant 1)))
                  )
              ]
          )
    it "le_true" $
      parseHelper "int main() {return 0 <= 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LessThanOrEqual (Constant 0) (Constant 2))
                  )
              ]
          )
    it "lt_false" $
      parseHelper "int main() {return 2 < 1;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LessThan (Constant 2) (Constant 1))
                  )
              ]
          )
    it "lt_true" $
      parseHelper "int main() {return 1 < 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LessThan (Constant 1) (Constant 2))
                  )
              ]
          )
    it "ne_false" $
      parseHelper "int main() {return 0 != 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Inequality (Constant 0) (Constant 0))
                  )
              ]
          )
    it "ne_true" $
      parseHelper "int main() {return -1 != -0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator Inequality (UnaryOperator Negation (Constant 1)) (UnaryOperator Negation (Constant 0)))
                  )
              ]
          )
    it "or_false" $
      parseHelper "int main() {return 0 || 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LogicalOr (Constant 0) (Constant 0))
                  )
              ]
          )
    it "or_true" $
      parseHelper "int main() {return 1 || 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      (BinaryOperator LogicalOr (Constant 1) (Constant 0))
                  )
              ]
          )
    it "precedence" $
      parseHelper "int main() {return 1 || 0 && 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          LogicalOr
                          (Constant 1)
                          (BinaryOperator LogicalAnd (Constant 0) (Constant 2))
                      )
                  )
              ]
          )
    it "precedence_2" $
      parseHelper "int main() {return (1 || 0) && 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          LogicalAnd
                          (BinaryOperator LogicalOr (Constant 1) (Constant 0))
                          (Constant 0)
                      )
                  )
              ]
          )
    it "precedence_3" $
      parseHelper "int main() {return 2 == 2 > 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          Equality
                          (Constant 2)
                          (BinaryOperator GreaterThan (Constant 2) (Constant 0))
                      )
                  )
              ]
          )
    it "precedence_4" $
      parseHelper "int main() {return 2 == 2 || 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Return
                      ( BinaryOperator
                          LogicalOr
                          (BinaryOperator Equality (Constant 2) (Constant 2))
                          (Constant 0)
                      )
                  )
              ]
          )
    it "skip_on_failure_multi_short_circuit" $
      parseHelper "int main() {int a = 0; a || (a = 3) || (a = 4); return a;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Expression
                      ( Just
                          ( BinaryOperator
                              LogicalOr
                              ( BinaryOperator
                                  LogicalOr
                                  (Variable "a")
                                  (Assign "a" (Constant 3))
                              )
                              (Assign "a" (Constant 4))
                          )
                      )
                  ),
                State (Return (Variable "a"))
              ]
          )
    it "skip_on_failure_short_circuit_and" $
      parseHelper "int main() {int a = 0;int b = 0;a && (b = 5);return b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Expression
                      ( Just
                          (BinaryOperator LogicalAnd (Variable "a") (Assign "b" (Constant 5)))
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "skip_on_failure_short_circuit_or" $
      parseHelper "int main() {int a = 1;int b = 0;a || (b = 5);return b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 1))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Expression
                      ( Just
                          ( BinaryOperator
                              LogicalOr
                              (Variable "a")
                              (Assign "b" (Constant 5))
                          )
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
  describe "week_5" $ do
    it "assign" $
      parseHelper "int main() {int a; a = 2; return a;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" Nothing),
                State (Expression (Just (Assign "a" (Constant 2)))),
                State (Return (Variable "a"))
              ]
          )
    it "assign_val" $
      parseHelper "int main() { int a; int b = a = 0; return b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" Nothing),
                Declaration (Declare "b" (Just (Assign "a" (Constant 0)))),
                State (Return (Variable "b"))
              ]
          )
    it "exp_return_val" $
      parseHelper "int main() { int a; int b; a = b = 4; return a - b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" Nothing),
                Declaration (Declare "b" Nothing),
                State
                  ( Expression
                      ( Just
                          ( Assign
                              "a"
                              ( Assign
                                  "b"
                                  (Constant 4)
                              )
                          )
                      )
                  ),
                State
                  ( Return
                      ( BinaryOperator
                          Subtraction
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
    it "initialize" $
      parseHelper "int main() { int a; int b; a = b = 4; return a - b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" Nothing),
                Declaration (Declare "b" Nothing),
                State
                  ( Expression
                      ( Just
                          ( Assign
                              "a"
                              ( Assign
                                  "b"
                                  (Constant 4)
                              )
                          )
                      )
                  ),
                State
                  ( Return
                      ( BinaryOperator
                          Subtraction
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
    it "missing_return" $
      parseHelper "int main() {}"
        `shouldBe` Program
          ( Fun
              "main"
              []
          )
    it "multiple_vars" $
      parseHelper "int main() { int a = 1; int b = 2; return a + b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 1))),
                Declaration (Declare "b" (Just (Constant 2))),
                State
                  ( Return
                      ( BinaryOperator
                          Addition
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
    it "no_initialize" $
      parseHelper "int main() { int a; return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" Nothing),
                State (Return (Constant 0))
              ]
          )
    it "refer" $
      parseHelper "int main() { int a = 2; return a;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 2))),
                State (Return (Variable "a"))
              ]
          )
    it "unused_exp" $
      parseHelper "int main() { 2 + 2; return 0;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ State
                  ( Expression
                      ( Just
                          ( BinaryOperator
                              Addition
                              (Constant 2)
                              (Constant 2)
                          )
                      )
                  ),
                State (Return (Constant 0))
              ]
          )
  describe "week_6" $ do
    it "assign_ternary" $
      parseHelper "int main() {int a = 0; a = 1 ? 2 : 3; return a;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Expression
                      ( Just
                          ( Assign
                              "a"
                              ( ConditionalExpression
                                  (Constant 1)
                                  (Constant 2)
                                  (Constant 3)
                              )
                          )
                      )
                  ),
                State (Return (Variable "a"))
              ]
          )
    it "multiple_ternary" $
      parseHelper "int main() { int a = 1 > 2 ? 3 : 4; int b = 1 > 2 ? 5 : 6; return a + b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (ConditionalExpression (BinaryOperator GreaterThan (Constant 1) (Constant 2)) (Constant 3) (Constant 4)))),
                Declaration (Declare "b" (Just (ConditionalExpression (BinaryOperator GreaterThan (Constant 1) (Constant 2)) (Constant 5) (Constant 6)))),
                State
                  ( Return
                      ( BinaryOperator
                          Addition
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
    it "nested_ternary" $
      parseHelper "int main() { int a = 1; int b = 2; int flag = 0; return a > b ? 5 : flag ? 6 : 7; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 1))),
                Declaration (Declare "b" (Just (Constant 2))),
                Declaration (Declare "flag" (Just (Constant 0))),
                State
                  ( Return
                      ( ConditionalExpression
                          (BinaryOperator GreaterThan (Variable "a") (Variable "b"))
                          (Constant 5)
                          ( ConditionalExpression
                              (Variable "flag")
                              (Constant 6)
                              (Constant 7)
                          )
                      )
                  )
              ]
          )
    it "nested_ternary_2" $
      parseHelper "int main() { int a = 1 ? 2 ? 3 : 4 : 5; int b = 0 ? 2 ? 3 : 4 : 5; return a * b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (ConditionalExpression (Constant 1) (ConditionalExpression (Constant 2) (Constant 3) (Constant 4)) (Constant 5)))),
                Declaration (Declare "b" (Just (ConditionalExpression (Constant 0) (ConditionalExpression (Constant 2) (Constant 3) (Constant 4)) (Constant 5)))),
                State
                  ( Return
                      ( BinaryOperator
                          Multiplication
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
    it "rh_assignment" $
      parseHelper "int main() {int a = 1 ? 2 ? 3 : 4 : 5; int b = 0 ? 2 ? 3 : 4 : 5; return a * b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (ConditionalExpression (Constant 1) (ConditionalExpression (Constant 2) (Constant 3) (Constant 4)) (Constant 5)))),
                Declaration (Declare "b" (Just (ConditionalExpression (Constant 0) (ConditionalExpression (Constant 2) (Constant 3) (Constant 4)) (Constant 5)))),
                State
                  ( Return
                      ( BinaryOperator
                          Multiplication
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
    it "ternary" $
      parseHelper "int main() { int a = 0; return a > -1 ? 4 : 5; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Return
                      ( ConditionalExpression
                          (BinaryOperator GreaterThan (Variable "a") (UnaryOperator Negation (Constant 1)))
                          (Constant 4)
                          (Constant 5)
                      )
                  )
              ]
          )
    it "ternary_short_circuit" $
      parseHelper "int main() { int a = 1; int b = 0; a ? (b = 1) : (b = 2); return b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 1))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Expression
                      ( Just
                          ( ConditionalExpression
                              (Variable "a")
                              (Assign "b" (Constant 1))
                              (Assign "b" (Constant 2))
                          )
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "ternary_short_circuit_2" $
      parseHelper "int main() { int a = 0; int b = 0; a ? (b = 1) : (b = 2); return b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Expression
                      ( Just
                          ( ConditionalExpression
                              (Variable "a")
                              (Assign "b" (Constant 1))
                              (Assign "b" (Constant 2))
                          )
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "else" $
      parseHelper "int main() { int a = 0; if (a) return 1; else return 2;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Conditional
                      (Variable "a")
                      (Return (Constant 1))
                      (Just (Return (Constant 2)))
                  )
              ]
          )
    it "if_nested" $
      parseHelper "int main() { int a = 1; int b = 0; if (a) b = 1; else if (b) b = 2; return b;}"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 1))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Conditional
                      (Variable "a")
                      (Expression (Just (Assign "b" (Constant 1))))
                      ( Just
                          ( Conditional
                              (Variable "b")
                              (Expression (Just (Assign "b" (Constant 2))))
                              Nothing
                          )
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "if_nested" $
      parseHelper "int main() { int a = 0; int b = 1; if (a) b = 1; else if (b) b = 2; return b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                Declaration (Declare "b" (Just (Constant 1))),
                State
                  ( Conditional
                      (Variable "a")
                      (Expression (Just (Assign "b" (Constant 1))))
                      ( Just
                          ( Conditional
                              (Variable "b")
                              (Expression (Just (Assign "b" (Constant 2))))
                              Nothing
                          )
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "if_nested" $
      parseHelper "int main() { int a = 0; int b = 1; if (a) b = 1; else if (b) b = 2; return b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                Declaration (Declare "b" (Just (Constant 1))),
                State
                  ( Conditional
                      (Variable "a")
                      (Expression (Just (Assign "b" (Constant 1))))
                      ( Just
                          ( Conditional
                              (Variable "b")
                              (Expression (Just (Assign "b" (Constant 2))))
                              Nothing
                          )
                      )
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "if_nested_3" $
      parseHelper "int main() { int a = 0; if (1) if (2) a = 3; else a = 4; return a; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Conditional
                      (Constant 1)
                      ( Conditional
                          (Constant 2)
                          (Expression (Just (Assign "a" (Constant 3))))
                          (Just (Expression (Just (Assign "a" (Constant 4)))))
                      )
                      Nothing
                  ),
                State (Return (Variable "a"))
              ]
          )
    it "if_nested_4" $
      parseHelper "int main() { int a = 0; if (1) if (0) a = 3; else a = 4; return a; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Conditional
                      (Constant 1)
                      ( Conditional
                          (Constant 0)
                          (Expression (Just (Assign "a" (Constant 3))))
                          (Just (Expression (Just (Assign "a" (Constant 4)))))
                      )
                      Nothing
                  ),
                State (Return (Variable "a"))
              ]
          )
    it "if_nested_5" $
      parseHelper "int main() { int a = 0; if (0) if (0) a = 3; else a = 4; else a = 1; return a; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                State
                  ( Conditional
                      (Constant 0)
                      ( Conditional
                          (Constant 0)
                          (Expression (Just (Assign "a" (Constant 3))))
                          (Just (Expression (Just (Assign "a" (Constant 4)))))
                      )
                      (Just (Expression (Just (Assign "a" (Constant 1)))))
                  ),
                State (Return (Variable "a"))
              ]
          )
    it "if_not_taken" $
      parseHelper "int main() { int a = 0; int b = 0; if (a) b = 1; return b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Conditional
                      (Variable "a")
                      (Expression (Just (Assign "b" (Constant 1))))
                      Nothing
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "if_taken" $
      parseHelper "int main() { int a = 1; int b = 0; if (a) b = 1; return b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 1))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Conditional
                      (Variable "a")
                      (Expression (Just (Assign "b" (Constant 1))))
                      Nothing
                  ),
                State (Return (Variable "b"))
              ]
          )
    it "multiple_if" $
      parseHelper "int main() { int a = 0; int b = 0; if (a) a = 2; else a = 3; if (b) b = 4; else b = 5; return a + b; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "a" (Just (Constant 0))),
                Declaration (Declare "b" (Just (Constant 0))),
                State
                  ( Conditional
                      (Variable "a")
                      (Expression (Just (Assign "a" (Constant 2))))
                      (Just (Expression (Just (Assign "a" (Constant 3)))))
                  ),
                State
                  ( Conditional
                      (Variable "b")
                      (Expression (Just (Assign "b" (Constant 4))))
                      (Just (Expression (Just (Assign "b" (Constant 5)))))
                  ),
                State
                  ( Return
                      ( BinaryOperator
                          Addition
                          (Variable "a")
                          (Variable "b")
                      )
                  )
              ]
          )
  describe "week 8" $ do
    it "break" $
      parseHelper "int main() { int sum = 0; for (int i = 0; i < 10; i = i + 1) { sum = sum + i; if (sum > 10) break; } return sum; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "sum" (Just (Constant 0))),
                State
                  ( ForDecl
                      (Declare "i" (Just (Constant 0)))
                      (BinaryOperator LessThan (Variable "i") (Constant 10))
                      ( Just
                          ( Assign
                              "i"
                              ( BinaryOperator
                                  Addition
                                  (Variable "i")
                                  (Constant 1)
                              )
                          )
                      )
                      ( Compound
                          [ State
                              ( Expression
                                  ( Just
                                      ( Assign
                                          "sum"
                                          ( BinaryOperator
                                              Addition
                                              (Variable "sum")
                                              (Variable "i")
                                          )
                                      )
                                  )
                              ),
                            State
                              ( Conditional
                                  ( BinaryOperator
                                      GreaterThan
                                      (Variable "sum")
                                      (Constant 10)
                                  )
                                  Break
                                  Nothing
                              )
                          ]
                      )
                  ),
                State (Return (Variable "sum"))
              ]
          )
    it "continue" $
      parseHelper "int main() { int sum = 0; for (int i = 0; i < 10; i = i + 1) { if ((sum / 2) * 2 != sum) continue; sum = sum + i; } return sum; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "sum" (Just (Constant 0))),
                State
                  ( ForDecl
                      (Declare "i" (Just (Constant 0)))
                      (BinaryOperator LessThan (Variable "i") (Constant 10))
                      (Just (Assign "i" (BinaryOperator Addition (Variable "i") (Constant 1))))
                      ( Compound
                          [ State
                              ( Conditional
                                  ( BinaryOperator
                                      Inequality
                                      ( BinaryOperator
                                          Multiplication
                                          (BinaryOperator Division (Variable "sum") (Constant 2))
                                          (Constant 2)
                                      )
                                      (Variable "sum")
                                  )
                                  Continue
                                  Nothing
                              ),
                            State
                              ( Expression
                                  ( Just
                                      ( Assign
                                          "sum"
                                          ( BinaryOperator
                                              Addition
                                              (Variable "sum")
                                              (Variable "i")
                                          )
                                      )
                                  )
                              )
                          ]
                      )
                  ),
                State (Return (Variable "sum"))
              ]
          )
    it "continue_empty_post" $
      parseHelper "int main() { int sum = 0; for (int i = 0; i < 10;) { i = i + 1; if (i == 2) continue; sum = sum + i; } return sum; }"
        `shouldBe` Program
          ( Fun
              "main"
              [ Declaration (Declare "sum" (Just (Constant 0))),
                State
                  ( ForDecl
                      (Declare "i" (Just (Constant 0)))
                      (BinaryOperator LessThan (Variable "i") (Constant 10))
                      Nothing
                      ( Compound
                          [ State
                              ( Expression
                                  ( Just
                                      ( Assign
                                          "i"
                                          ( BinaryOperator
                                              Addition
                                              (Variable "i")
                                              (Constant 1)
                                          )
                                      )
                                  )
                              ),
                            State
                              ( Conditional
                                  ( BinaryOperator Equality (Variable "i") (Constant 2)
                                  )
                                  Continue
                                  Nothing
                              ),
                            State
                              ( Expression
                                  ( Just
                                      ( Assign
                                          "sum"
                                          ( BinaryOperator
                                              Addition
                                              (Variable "sum")
                                              (Variable "i")
                                          )
                                      )
                                  )
                              )
                          ]
                      )
                  ),
                State (Return (Variable "sum"))
              ]
          )