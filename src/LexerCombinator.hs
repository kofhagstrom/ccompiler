module LexerCombinator
  ( Keyword (..),
    Token (..),
    Literal (..),
    lexNextToken,
    lexFile,
    LexError (..),
  )
where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Data.Char (isAlpha, isDigit)
import Data.Functor (($>))
import Data.Monoid (Any (Any, getAny))
import Parser (Parser (Parser))
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
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

data LexError = UnexpectedError deriving (Show)

type Lexer a = Parser String [LexError] a

stringToToken :: [(String, Token)]
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

spanL :: (Char -> Bool) -> Lexer String
spanL f = Parser $ \input ->
  let (str, rest) = span f input
   in Right (str, rest)

ws :: Lexer String
ws = spanL (== ' ')

lexChar :: Char -> Lexer Char
lexChar t = Parser f
  where
    f str@(c : rest) =
      if c == t
        then Right (c, rest)
        else Left ([UnexpectedError], str)
    f [] = Left ([UnexpectedError], [])

lexString :: String -> Lexer String
lexString = traverse lexChar

isAllowedLiteralChar :: Char -> Bool
isAllowedLiteralChar = getAny . foldMap (Any .) predicates
  where
    predicates = [isAlpha, flip elem "_"]

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

lexNonLiteral :: [(String, Token)] -> Parser String [LexError] Token
lexNonLiteral ((str, t) : rest) = (lexString str >> return t) <|> lexNonLiteral rest
lexNonLiteral [] = empty

lexComment :: Lexer ()
lexComment = (lexString "//" *> spanL (/= '\n')) $> ()

lexMultiLineComment :: Lexer ()
lexMultiLineComment = (lexString "/*" *> spanL (/= '*') *> lexString "*/") $> ()

lexNewline :: Lexer ()
lexNewline = lexChar '\n' $> ()

lexNextToken :: Lexer Token
lexNextToken =
  ( ( lexNewline
        <|> lexComment
        <|> lexMultiLineComment
    )
      >> lexNextToken
  )
    <|> (ws *> lexNonLiteral stringToToken <* ws)
    <|> lexLiteral

lexFile :: Lexer [Token]
lexFile = many lexNextToken
