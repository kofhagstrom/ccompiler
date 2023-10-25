module Generator (generateAssembly) where

import ParserCombinator

newtype AssemblyProgram = AssemblyProgram [AssemblyFunction]

instance Show AssemblyProgram where
  show (AssemblyProgram functions) = unlines (map show functions)

data AssemblyFunction = AssemblyFunction String [Instruction]

instance Show AssemblyFunction where
  show (AssemblyFunction funcName instructions) =
    unlines
      ( [ ".globl _" ++ funcName,
          "_" ++ funcName ++ ":"
        ]
          ++ map show instructions
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
  | Neg Arg
  | Push Arg
  | Pop Arg
  | Ret
  deriving (Eq)

instance Show Operation where
  show (Mov a1 a2) = "movl    " ++ a1 ++ ", " ++ a2
  show (Cmp a1 a2) = "cmp    " ++ a1 ++ ", " ++ a2
  show (Cset a1 a2) = "cset   " ++ a1 ++ ", " ++ a2
  show (Add a1 a2 a3) = "add    " ++ a1 ++ ", " ++ a2 ++ ", " ++ a3
  show (Neg a1) = "neg    " ++ a1
  show (Push a1) = "push   " ++ a1
  show (Pop a1) = "pop    " ++ a1
  show Ret = "ret"

data AssemblyError = InvalidAssembly deriving (Show)

generateAssembly :: Program -> AssemblyProgram
generateAssembly (Program [F (Fun funcName _ (Just statements))]) =
  AssemblyProgram
    [ AssemblyFunction
        funcName
        (concatMap generateBlockItem statements)
    ]
generateAssembly _ = error "Invalid program"

generateBlockItem :: BlockItem -> [Instruction]
generateBlockItem (State statement) =
  generateStatement statement
generateBlockItem _ = error "Invalid block item"

generateStatement :: Statement -> [Instruction]
generateStatement (Return expr) =
  generateExpression expr ++ [Instruction Ret]
generateStatement _ = error "Invalid statement"

generateExpression :: Expression -> [Instruction]
generateExpression (Constant value) =
  [ Instruction (Mov ("$" ++ show value) "%eax")
  ]
generateExpression (UnaryOperator Negation expr) =
  generateExpression
    expr
    ++ [Instruction (Neg "%eax")]
generateExpression (UnaryOperator LogicalNegation expr) =
  generateExpression expr
    ++ [ Instruction (Cmp "x0" "0"),
         Instruction (Cset "x0" "eq")
       ]
generateExpression (BinaryOperator Addition expr1 expr2) =
  generateExpression expr1
    ++ [Instruction (Push "SP")]
    ++ generateExpression expr2
    ++ [Instruction (Pop "SP")]
    ++ [Instruction (Add "x0" "x0" "x1")]
generateExpression _ = error "Invalid expression"
