module Main
  ( main,
  )
where

import Compiler (runLexer, runParser)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      fileContents <- readFile file
      runParser fileContents
    [file, mode] -> do
      fileContents <- readFile file
      case mode of
        "lex" -> runLexer fileContents
        "parse" -> runParser fileContents
        _ -> putStrLn "Invalid mode"
    _ -> putStrLn "Wrong number of arguments"
