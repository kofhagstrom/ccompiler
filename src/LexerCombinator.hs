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
  | CommentT String
  | KeywordT Keyword
  | LiteralT Literal
  deriving (Show, Eq)

data Keyword
  = IntKW
  | ReturnKW
  | IfKW
  | ElseKW
  | ForKW
  | DoKW
  | WhileKW
  | BreakKW
  | ContinueKW
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
    ("for", KeywordT ForKW),
    ("while", KeywordT WhileKW),
    ("do", KeywordT DoKW),
    ("break", KeywordT BreakKW),
    ("continue", KeywordT ContinueKW),
    ("==", LogicalEqualityT),
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
isAllowedLiteralChar c = isAlpha c || isIn c "_"
  where
    isIn c' (l : ls) = (l == c') || isIn c' ls
    isIn _ [] = False

lexStringLiteral :: Lexer Token
lexStringLiteral =
  Parser
    ( \input -> case span isAllowedLiteralChar input of
        ("", rest) -> Left ([], rest)
        (str, rest) -> Right (LiteralT (IdentifierL str), rest)
    )

lexIntLiteral :: Lexer Token
lexIntLiteral =
  Parser
    ( \input -> case span isDigit input of
        (str, rest) -> case readEither str of
          Right i -> Right (LiteralT (IntL i), rest)
          Left _ -> Left ([UnexpectedError], rest)
    )

lexLiteral :: Lexer Token
lexLiteral = ws *> (lexStringLiteral <|> lexIntLiteral) <* ws

lexNonLiteral :: [(String, Token)] -> Parser Char LexError Token
lexNonLiteral ((str, t) : rest) = (lexString str >> return t) <|> lexNonLiteral rest
lexNonLiteral [] = empty

lexComment :: Lexer ()
lexComment = do
  lexString "//"
  spanL (/= '\n')
  return ()

lexNewline :: Lexer ()
lexNewline = do
  lexChar '\n'
  return ()

lexNextToken :: Lexer Token
lexNextToken =
  ((lexNewline <|> lexComment) >> lexNextToken)
    <|> (ws *> lexNonLiteral stringToToken <* ws)
    <|> lexLiteral

lexFile :: Lexer [Token]
lexFile = many lexNextToken
