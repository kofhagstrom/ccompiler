module LLVMGenerator (generateLLVMIR) where

import Parser
  ( BinaryOperator (Addition),
    BlockItem (State),
    Expression (BinOp, Constant),
    FuncDeclaration (..),
    Program (..),
    Statement (Expression, Return),
  )
import Text.Printf (printf)

newtype LLVMProgram = LLVMProgram [LLVMFunction]

instance Show LLVMProgram where
  show (LLVMProgram functions) = unlines (map show functions)

data LLVMFunction = LLVMFunction String [Instruction]

instance Show LLVMFunction where
  show (LLVMFunction funcName instructions) =
    unlines
      ( printf "define i32 @%s() {" funcName : map (("  " ++) . show) instructions ++ ["}"]
      )

newtype Instruction = Instruction Operation deriving (Eq)

instance Show Instruction where
  show (Instruction op) = show op

data Operation
  = Ret Arg
  | Add Arg Arg Arg
  deriving (Eq)

instance Show Operation where
  show (Ret arg) = printf "ret i32 %s" (show arg)
  show (Add arg1 arg2 arg3) = printf "%s = add i32 %s, %s" (show arg1) (show arg2) (show arg3)

data Arg = ConstantArg Integer | Register String deriving (Eq)

instance Show Arg where
  show (ConstantArg value) = show value
  show (Register name) = printf "%%%s" name

generateLLVMIR :: Program -> LLVMProgram
generateLLVMIR
  ( Program
      function
    ) =
    LLVMProgram [generateLLVMFunction function]

generateLLVMFunction :: FuncDeclaration -> LLVMFunction
generateLLVMFunction
  ( Fun
      funcName
      [ State (Return statement)
        ]
    ) =
    LLVMFunction
      funcName
      $ generateLLVMInstructions statement
generateLLVMFunction _ = error "Invalid function"

generateLLVMInstructions :: Expression -> [Instruction]
generateLLVMInstructions (Constant value) =
  [Instruction $ Ret (ConstantArg value)]
generateLLVMInstructions (BinOp Addition (Constant c1) (Constant c2)) =
  [ Instruction $ Add (Register "1") (ConstantArg c1) (ConstantArg c2)
  ]
generateLLVMInstructions
  (BinOp Addition expr (Constant c3)) =
    generateLLVMInstructions expr
      ++ [ Instruction $ Add (Register "2") (Register "1") (ConstantArg c3),
           Instruction $ Ret (Register "2")
         ]
generateLLVMInstructions _ = error "Invalid instruction"