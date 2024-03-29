module Lexer
  ( Keyword (..),
    Token (..),
    Literal (..),
    token,
    tokens,
    whitespace,
    stringLiteral,
    intLiteral,
    literal,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Parsec (Parser, ignore, manyOf, noneOf, orElse, parse, parseWhile, skip)

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
  | CommaT
  | CommentT String
  | KeywordT Keyword
  | LiteralT Literal
  deriving (Show, Eq)

data Keyword
  = IntKW
  | StringKW
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
  = IntL String
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

type Lexer a = Parser String a

whitespace :: Lexer String
whitespace = parseWhile (== ' ')

string :: Lexer Token
string = do
  skip "\""
  o <- noneOf "\""
  skip "\""
  return . LiteralT . IdentifierL $ o

stringLiteral :: Lexer Token
stringLiteral = do
  o <- manyOf "abcdefghijklmnopqrstuvxyz_"
  return . LiteralT . IdentifierL $ o

intLiteral :: Lexer Token
intLiteral = do
  o <- manyOf "1234567890"
  return . LiteralT . IntL $ o

literal :: Lexer Token
literal = do
  ignore whitespace
  output <- stringLiteral `orElse` intLiteral `orElse` string
  ignore whitespace
  return output

nonLiteral :: Parser [Char] Token
nonLiteral = nonLiteral' stringToToken
  where
    stringToToken =
      [ ("int", KeywordT IntKW),
        ("string", KeywordT StringKW),
        ("if", KeywordT IfKW),
        ("else", KeywordT ElseKW),
        ("return", KeywordT ReturnKW),
        ("for", KeywordT ForKW),
        ("while", KeywordT WhileKW),
        ("do", KeywordT DoKW),
        ("break", KeywordT BreakKW),
        ("continue", KeywordT ContinueKW),
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
        (":", ColonT),
        (",", CommaT)
      ]
    nonLiteral' ((str, t) : rest) =
      ( do
          ignore $ parse str
          return t
      )
        <|> nonLiteral' rest
    nonLiteral' [] = empty

comment :: Lexer String
comment = do
  skip "//"
  parseWhile (/= '\n')

multilineComment :: Lexer String
multilineComment = do
  skip "/*"
  o <- noneOf "*"
  skip "*/"
  return o

newLine :: Lexer String
newLine = parse "\n"

token :: Lexer Token
token =
  ( do
      ignore $ newLine <|> comment <|> multilineComment
      token
  )
    `orElse` ( do
                 ignore whitespace
                 output <- nonLiteral
                 ignore whitespace
                 return output
             )
    `orElse` literal

tokens :: Lexer [Token]
tokens = many token