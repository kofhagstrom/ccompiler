module Main
  ( main,
  )
where

import LexerCombinator
import Parser
import ParserCombinator
import System.Environment (getArgs)

runLexer :: String -> IO ()
runLexer fileContents = case run lexFile fileContents of
  Right (out, _) -> print out
  Left (es, _) -> except es
  where
    except es = error $ concatMap show es

runParser :: String -> IO ()
runParser = either except handleParse . run lexFile
  where
    handleParse (out, _) = either except (print . fst) . run parseProgram $ out
    except es = error $ concatMap show es

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
