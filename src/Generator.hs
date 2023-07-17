module Generator (generateAssembly) where

import Parser (Expression (..), FuncDeclaration (..), Program (..), Statement (..))

generateAssembly :: Program -> String
generateAssembly (Program (Fun funcName (Return (Constant value)))) =
  unlines
    [ "    .globl _" ++ funcName,
      "_" ++ funcName ++ ":",
      "    movl    $" ++ show value ++ ", %eax",
      "    ret"
    ]