{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module ParserCombinator
  ( Parser,
  )
where

import Control.Applicative
  ( Alternative (empty, (<|>)),
  )
import Lexer
  ( Keyword (IntKW, ReturnKW),
    Literal (IdentifierL, IntL),
    Token (..),
  )

-- Expression ::= Constant Integer | Variable String
data Expression
  = Constant Integer
  | Variable String
  deriving (Show, Eq)

-- Statement ::= "return" Expression ";"
newtype Statement = Return Expression deriving (Show, Eq)

-- FuncDeclaration ::= "int" Identifier "(" ")" "{" Statement "}"
data FuncDeclaration = Fun String Statement deriving (Show, Eq)

-- Program ::= FuncDeclaration
newtype Program = Program FuncDeclaration deriving (Show, Eq)

data ParseError = UnexpectedError deriving (Show)

newtype Parser a = Parser {runParser :: [Token] -> Either [ParseError] (a, [Token])}

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (x, input') <- runParser p input
    Right (f x, input')

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  p1 <*> p2 = Parser $ \input -> do
    (f, input') <- runParser p1 input
    (a, input'') <- runParser p2 input'
    Right (f a, input'')

instance Alternative Parser where
  empty = Parser $ \_ -> Left []
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Right a -> Right a
      Left e -> case runParser p2 input of
        Right a' -> Right a'
        Left e' -> Left (e ++ e')

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \input -> do
    (a, input') <- runParser p input
    runParser (f a) input'

parseT :: Token -> Parser ()
parseT t = Parser f
  where
    f (t' : ts)
      | t' == t = Right ((), ts)
      | otherwise = Left [UnexpectedError]
    f [] = Left [UnexpectedError]

parseTs :: [Token] -> Parser [()]
parseTs = traverse parseT

parseIdentifier :: Parser String
parseIdentifier = Parser $ \case
  (LiteralT (IdentifierL identifier) : ts) -> Right (identifier, ts)
  (LiteralT (IntL value) : ts) -> Right (value, ts)
  _ -> Left [UnexpectedError]

parseExpression :: Parser Expression
parseExpression = parseConstantExpression <|> parseVariableExpression

parseConstantExpression :: Parser Expression
parseConstantExpression = Constant . read <$> parseIdentifier

parseVariableExpression :: Parser Expression
parseVariableExpression = Variable <$> parseIdentifier

-- Statement ::= "return" Expression ";"
parseStatement :: Parser Statement
parseStatement =
  parseTs [KeywordT ReturnKW]
    *> (Return <$> parseExpression <* parseTs [SemiColonT])

-- FuncDeclaration ::= "int" Identifier "(" ")" "{" Statement "}"
parseFuncDeclaration :: Parser FuncDeclaration
parseFuncDeclaration = do
  parseTs [KeywordT IntKW]
  identifier <- parseIdentifier
  parseTs [OpenParenthesisT, CloseParenthesisT, OpenBraceT]
  func <- Fun identifier <$> parseStatement
  parseTs [CloseBraceT]
  return func

parseProgram :: Parser Program
parseProgram = Program <$> parseFuncDeclaration