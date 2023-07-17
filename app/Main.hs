module Main
  ( main,
  )
where

import Generator ()
import Lexer (lexChars)
import Parser (parseAST)
import System.Environment

run :: String -> IO ()
run string =
  case lexing of
    Left err -> print err
    Right tokens -> print $ (tokens)
  where
    -- putStr $ generateAssembly (parseAST tokens)

    lexing = lexChars $ lines string

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- readFile file
      run x
    _ -> putStrLn "Wrong number of arguments"
