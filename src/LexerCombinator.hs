{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LexerCombinator
  ( Keyword (..),
    Token (..),
    Literal (..),
    lexNextToken,
    lexFile,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Data.Char (isAlpha, isDigit)
import Parser
import Text.Read (readEither)

data Token
  = OpenBraceT
  | CloseBraceT
  | OpenParenthesisT
  | CloseParenthesisT
  | SemiColonT
  | PlusT
  | AsteriskT
  | DivisionT
  | MinusT
  | BangT
  | TildeT
  | LogicalEqualityT
  | InequalityT
  | LessThanT
  | GreaterThanT
  | GreaterThanOrEqualT
  | LessThanOrEqualT
  | NotEqualT
  | AndT
  | OrT
  | AssignmentT
  | ColonT
  | QuestionMarkT
  | KeywordT Keyword
  | LiteralT Literal
  | ErrorT String
  deriving (Show, Eq)

data Keyword
  = IntKW
  | ReturnKW
  | IfKW
  | ElseKW
  deriving (Show, Eq)

data Literal
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

data LexError = UnexpectedError deriving (Show)

type Lexer a = Parser Char LexError a

stringToToken :: [(String, Token)]
stringToToken =
  [ ("int", KeywordT IntKW),
    ("if", KeywordT IfKW),
    ("else", KeywordT ElseKW),
    ("return", KeywordT ReturnKW),
    ("==", LogicalEqualityT),
    ("=", AssignmentT),
    ("!=", NotEqualT),
    ("!", BangT),
    ("<=", LessThanOrEqualT),
    ("<", LessThanT),
    (">=", GreaterThanOrEqualT),
    (">", GreaterThanT),
    ("&&", AndT),
    ("||", OrT),
    ("(", OpenParenthesisT),
    (")", CloseParenthesisT),
    ("{", OpenBraceT),
    ("}", CloseBraceT),
    (";", SemiColonT),
    ("-", MinusT),
    ("~", TildeT),
    ("+", PlusT),
    ("*", AsteriskT),
    ("/", DivisionT),
    ("?", QuestionMarkT),
    (":", ColonT)
  ]

spanL :: (Char -> Bool) -> Lexer String
spanL f = Parser $ \input ->
  let (str, rest) = span f input
   in Right (str, rest)

ws :: Lexer String
ws = spanL (== ' ')

lexChar :: Char -> Lexer Char
lexChar t = Parser f
  where
    f (c : str) =
      if c == t
        then Right (c, str)
        else Left ([UnexpectedError], str)
    f [] = Left ([UnexpectedError], [])

lexString :: String -> Lexer String
lexString = traverse lexChar

isAllowedLiteralChar :: Char -> Bool
isAllowedLiteralChar c = isAlpha c || c == '_'

lexLiteralT :: Lexer Token
lexLiteralT =
  ws
    *> ( Parser
           ( \input -> case span isAllowedLiteralChar input of
               ("", rest) -> Left ([], rest)
               (str, rest) -> Right (LiteralT (IdentifierL str), rest)
           )
           <|> Parser
             ( \input -> case span isDigit input of
                 ("", rest) -> Left ([], rest)
                 (str, rest) -> case readEither str of
                   Right i -> Right (LiteralT (IntL i), rest)
                   Left _ -> Left ([UnexpectedError], rest)
             )
       )
    <* ws

lexNonLiterals :: [(String, Token)] -> Parser Char LexError Token
lexNonLiterals ((str, t) : rest) = (lexString str >> return t) <|> lexNonLiterals rest
lexNonLiterals [] = empty

lexNextToken :: Lexer Token
lexNextToken = (ws *> lexNonLiterals stringToToken <* ws) <|> lexLiteralT

lexFile :: Lexer [Token]
lexFile = many lexNextToken
