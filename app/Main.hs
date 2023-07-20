module Main
  ( main,
  )
where

import Generator (generateAssembly)
import Lexer (Token, lexChars)
import Parser (parseAST)
import System.Environment (getArgs)

run :: String -> ([Token] -> IO ()) -> IO ()
run string f =
  case lexing of
    Left err -> print err
    Right tokens -> f tokens
  where
    lexing = lexChars $ lines string

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- readFile file
      run x (putStr . show . generateAssembly . parseAST)
    [file, mode] -> do
      x <- readFile file
      case mode of
        "lex" -> run x print
        "parse" -> run x (print . parseAST)
        "assemble" -> run x (putStr . show . generateAssembly . parseAST)
        _ -> putStrLn "Invalid mode"
    _ -> putStrLn "Wrong number of arguments"
