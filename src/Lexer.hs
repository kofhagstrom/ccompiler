{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Lexer
  ( lexChars,
    Lexer,
    Keyword (..),
    Token (..),
    Literal (..),
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.Map (Map, fromList, lookup)

data Token
  = Space
  | OpenBrace
  | CloseBrace
  | OpenParenthesis
  | CloseParenthesis
  | SemiColon
  | Plus
  | Asterisk
  | Division
  | Minus
  | Bang
  | Tilde
  | Keyword Keyword
  | Literal Literal
  | Error String
  deriving (Show, Eq)

data Keyword
  = Int
  | Return
  deriving (Show, Eq)

data Literal
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

type LexRow = String -> [Token]

keywords :: Map String Keyword
keywords =
  fromList
    [ ("return", Return)
    ]

singleCharTokens :: Map Char Token
singleCharTokens =
  fromList
    [ (' ', Space),
      ('(', OpenParenthesis),
      (')', CloseParenthesis),
      ('{', OpenBrace),
      ('}', CloseBrace),
      (';', SemiColon),
      ('-', Minus),
      ('!', Bang),
      ('~', Tilde),
      ('+', Plus),
      ('*', Asterisk),
      ('/', Division)
    ]

loxMultiCharTokens :: Map Char LexRow
loxMultiCharTokens =
  fromList
    [ ('/', lexSlash)
    ]

type Lexer = [String] -> Either String [Token]

lexChars :: Lexer
lexChars strings = sequence . checkForErrors $ map lexRow strings

lexRow :: LexRow
lexRow string =
  if null string
    then []
    else case token of
      Just tok ->
        if tok == Space
          then lexRow rest
          else tok : lexRow rest
      Nothing -> lexMultiCharToken string
  where
    (char : rest) = string
    token = Data.Map.lookup char singleCharTokens

lexMultiCharToken :: LexRow
lexMultiCharToken string =
  case maybeFun of
    Just fun -> fun rest
    Nothing -> lexRest
  where
    (char : rest) = string
    maybeFun = Data.Map.lookup char loxMultiCharTokens
    lexRest
      | isDigit char = lexIntegerLiteral string
      | isAlphaNum char = lexIdentifiersAndKeywords string
      | otherwise = [Error "Invalid token."]

lexSlash :: LexRow
lexSlash [] = undefined
lexSlash string@(x : _)
  | x == '/' = []
  | otherwise = Division : lexRow string

lexIntegerLiteral :: LexRow
lexIntegerLiteral string = (Literal . IntL . read) prefix : lexRow suffix
  where
    (prefix, suffix) = span isAlphaNum string

lexIdentifiersAndKeywords :: LexRow
lexIdentifiersAndKeywords string =
  case keyword of
    Just kw -> Keyword kw : lexRow suffix
    Nothing -> Literal (IdentifierL prefix) : lexRow suffix
  where
    keyword = Data.Map.lookup prefix keywords
    (prefix, suffix) = span isAlphaNum string

type ErrorMessage = String

checkForErrors :: [[Token]] -> [Either ErrorMessage Token]
checkForErrors [] = []
checkForErrors tokens = concatMap checkRow (zip [1 ..] tokens)

checkRow :: (Integer, [Token]) -> [Either ErrorMessage Token]
checkRow (_, []) = []
checkRow (n, t : ts) =
  let rest = (n, ts)
   in case t of
        Error err -> Left ("Error on line " <> show n <> ": " ++ err) : checkRow rest
        _ -> Right t : checkRow rest