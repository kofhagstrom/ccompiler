module Generator (generateAssembly) where

import Parser (Expression (..), FuncDeclaration (..), Program (..), Statement (..))

generateAssembly :: Program -> String
generateAssembly (Program (Fun funcName (Return (Constant value) : _))) =
  unlines
    [ ".global _" ++ funcName,
      "",
      ".balign 4",
      "",
      "_" ++ funcName ++ ":",
      "    mov    x0, " ++ show value,
      "    ret"
    ]