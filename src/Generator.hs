module Generator (generateAssembly) where

import Parser
  ( BinaryOperator (Addition),
    Expression (BinOp, Constant, UnOp),
    FuncDeclaration (Fun),
    Program (Program),
    Statement (Return),
    UnitaryOperator (LogicalNegation, Negation),
  )

newtype Instruction = Instruction Operation deriving (Eq)

instance Show Instruction where
  show (Instruction op) = "    " ++ show op

type Arg = String

data Operation
  = Mov Arg Arg
  | Cmp Arg Arg
  | Cset Arg Arg
  | Add Arg Arg Arg
  | Neg Arg Arg
  deriving (Eq)

instance Show Operation where
  show (Mov a1 a2) = "mov    " ++ a1 ++ ", " ++ a2
  show (Cmp a1 a2) = "cmp    " ++ a1 ++ ", " ++ a2
  show (Cset a1 a2) = "cset   " ++ a1 ++ ", " ++ a2
  show (Add a1 a2 a3) = "add    " ++ a1 ++ ", " ++ a2 ++ ", " ++ a3
  show (Neg a1 a2) = "neg    " ++ a1 ++ ", " ++ a2

generateAssembly :: Program -> String
generateAssembly (Program (Fun funcName statements)) =
  unlines
    ( [ ".global _" ++ funcName,
        ".balign 4",
        "_" ++ funcName ++ ":"
      ]
        ++ concatMap generateStatement statements
    )

generateStatement :: Statement -> [String]
generateStatement (Return expr) = map show (generateExpression expr) ++ ["    ret"]
generateStatement statemtent = map show (generateStatement statemtent)

generateExpression :: Expression -> [Instruction]
generateExpression (Constant value) =
  [ Instruction (Mov "x0" (show value))
  ]
generateExpression (UnOp Negation expr) =
  generateExpression
    expr
    ++ [Instruction (Neg "x0" "x0")]
generateExpression (UnOp LogicalNegation expr) =
  generateExpression expr
    ++ [ Instruction (Cmp "x0" "0"),
         Instruction (Cset "x0" "eq")
       ]
generateExpression (BinOp Addition expr1 expr2) =
  generateExpression expr1
    ++ [Instruction (Mov "x1" "x0")]
    ++ generateExpression expr2
    ++ [ Instruction (Add "x0" "x0" "x1")
       ]
generateExpression _ = error "Invalid expression"