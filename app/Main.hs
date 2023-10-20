module Main
  ( main,
  )
where

import LexerCombinator
import Parser
import ParserCombinator
import System.Environment (getArgs)

runLexer :: String -> IO ()
runLexer fileContents = case run lexFile input of
  Right (out, _) -> print out
  Left (es, _) -> error (concatMap show es)
  where
    input = fileContents

runParser :: String -> IO ()
runParser fileContents =
  either (error . concatMap show) handleParse . run lexFile $ input
  where
    input = fileContents
    handleParse (out, _) = either (error . concatMap show) (print . fst) . run parseProgram $ out

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
