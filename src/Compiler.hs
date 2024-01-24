module Compiler (runLexer, runParser) where

import LexerCombinator
import ParserCombinator

runLexer :: String -> IO ()
runLexer fileContents = case run tokens fileContents of
  Right (out, _) -> print out
  Left (es, _) -> except es
  where
    except es = error $ concatMap show es

runParser :: String -> IO ()
runParser = either except handleParse . run tokens
  where
    handleParse (out, _) = either except (print . fst) . run parseProgram $ out
    except es = error $ concatMap show es