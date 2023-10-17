{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module LexerCombinator
  ( Lexer (..),
    Keyword (..),
    Token (..),
    Literal (..),
    lexNextToken,
    lexFile,
  )
where

import Control.Applicative (Alternative (empty, (<|>)), many)
import Data.Char (isAlpha, isDigit)

data Token
  = EOF
  | OpenBraceT
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

newtype Lexer a = Lexer {runLexer :: String -> Either ([LexError], String) (a, String)}

instance Functor Lexer where
  fmap f p = Lexer $ \input -> do
    (x, input') <- runLexer p input
    return (f x, input')

instance Applicative Lexer where
  pure a = Lexer $ \input -> Right (a, input)
  p1 <*> p2 = Lexer $ \input -> do
    (f, input') <- runLexer p1 input
    (a, input'') <- runLexer p2 input'
    return (f a, input'')

instance Alternative Lexer where
  empty = Lexer $ \_ -> Left ([UnexpectedError], [])
  p1 <|> p2 = Lexer $ \input ->
    case runLexer p1 input of
      Right a -> Right a
      Left (e, _) -> case runLexer p2 input of
        Right a' -> Right a'
        Left (e', ts') -> Left (e <> e', ts')

instance Monad Lexer where
  return = pure
  p >>= f = Lexer $ \input -> do
    (a, input') <- runLexer p input
    runLexer (f a) input'

stringToToken :: [(String, Token)]
stringToToken =
  [ ("int", KeywordT IntKW),
    ("if", KeywordT IfKW),
    ("else", KeywordT ElseKW),
    ("return", KeywordT ReturnKW),
    ("==", LogicalEqualityT),
    ("!=", NotEqualT),
    ("!", BangT),
    ("<=", LessThanOrEqualT),
    ("<", LessThanT),
    (">=", GreaterThanOrEqualT),
    (">", GreaterThanT),
    ("&&", AndT),
    ("||", OrT),
    ("=", AssignmentT),
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
spanL f = Lexer $ \input ->
  let (str, rest) = span f input
   in Right (str, rest)

ws :: Lexer String
ws = spanL (== ' ')

lexChar :: Char -> Lexer Char
lexChar t = Lexer f
  where
    f (c : str) =
      if c == t
        then Right (c, str)
        else Left ([UnexpectedError], str)
    f [] = Left ([UnexpectedError], [])

lexString :: String -> Lexer String
lexString = traverse lexChar

isAllowedLiteralChar :: Char -> Bool
isAllowedLiteralChar c = case c of
  '_' -> True
  _ -> isAlpha c

lexLiteralT :: Lexer Token
lexLiteralT =
  ws
    *> ( Lexer
           ( \input -> case span isAllowedLiteralChar input of
               ("", rest) -> Left ([], rest)
               (str, rest) -> Right (LiteralT (IdentifierL str), rest)
           )
           <|> Lexer
             ( \input -> case span isDigit input of
                 ("", rest) -> Left ([], rest)
                 (str, rest) -> Right (LiteralT (IntL (read str)), rest)
             )
       )
    <* ws

lexNextToken :: Lexer Token
lexNextToken = (ws *> (f stringToToken) <* ws) <|> lexLiteralT
  where
    f ((str, t) : rest) =
      (lexString str >> return t)
        <|> f rest
    f [] = empty

lexFile :: Lexer [Token]
lexFile = many lexNextToken