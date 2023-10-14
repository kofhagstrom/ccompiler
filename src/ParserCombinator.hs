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

data UnaryOperator
  = Negation
  | LogicalNegation
  | BitwiseComplement
  deriving (Show, Eq)

data BinaryOperator
  = Addition
  | Multiplication
  | Division
  deriving (Show, Eq)

-- <exp> ::= <exp> <binary_op> <exp> | <unary_op> <exp> | "(" <exp> ")" | <int> | "var" <str>
data Expression
  = BinOp BinaryOperator Expression Expression
  | UnOp UnaryOperator Expression
  | Constant Integer
  | Variable String
  deriving (Show, Eq)

-- <statement> ::= "return" <exp> ";"
newtype Statement = Return Expression deriving (Show, Eq)

-- <function> ::= "int" <id> "(" ")" "{" <statement> "}"
data FuncDeclaration = Fun String Statement deriving (Show, Eq)

-- <program> ::= <function>
newtype Program = Program FuncDeclaration deriving (Show, Eq)

data ParseError = Error String | UnexpectedError deriving (Show)

newtype Parser a = Parser {runParser :: [Token] -> Either [ParseError] (a, [Token])}

instance Functor Parser where
  fmap f p = Parser $ \input -> do
    (x, input') <- runParser p input
    return (f x, input')

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  p1 <*> p2 = Parser $ \input -> do
    (f, input') <- runParser p1 input
    (a, input'') <- runParser p2 input'
    return (f a, input'')

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

parseT :: Token -> Parser Token
parseT t = Parser f
  where
    f (t' : ts)
      | t' == t = Right (t', ts)
      | otherwise = Left [Error ("Expected " ++ show t ++ ", got " ++ show t')]
    f [] = Left [UnexpectedError]

parseTs :: [Token] -> Parser [Token]
parseTs = traverse parseT

parseIdentifierLiteral :: Parser String
parseIdentifierLiteral = Parser $ \case
  (LiteralT (IdentifierL identifier) : ts) -> Right (identifier, ts)
  _ -> Left [UnexpectedError]

parseIntLiteral :: Parser Integer
parseIntLiteral = Parser $ \case
  (LiteralT (IntL value) : ts) -> Right (value, ts)
  _ -> Left [UnexpectedError]

parseUnaryOperation :: Parser Expression
parseUnaryOperation =
  (parseTs [BangT] *> (UnOp LogicalNegation <$> parseExpression))
    <|> (parseTs [MinusT] *> (UnOp Negation <$> parseExpression))
    <|> (parseTs [TildeT] *> (UnOp LogicalNegation <$> parseExpression))

parseBinaryOperation :: Parser Expression
parseBinaryOperation = do
  expr <- parseExpression
  parseTs [AsteriskT]
  BinOp Multiplication expr <$> parseExpression

-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
parseFactor :: Parser Expression
parseFactor =
  (parseTs [OpenParenthesisT] *> parseExpression <* parseTs [CloseParenthesisT])
    <|> parseUnaryOperation
    <|> (Constant <$> parseIntLiteral)

-- <term> ::= <factor> { ("*" | "/") <factor> }
parseTerm :: Parser Expression
parseTerm =
  ( do
      factor <- parseFactor
      parseTs [AsteriskT]
      BinOp Multiplication factor <$> parseFactor
  )
    <|> ( do
            factor <- parseFactor
            parseTs [DivisionT]
            BinOp Division factor <$> parseFactor
        )
    <|> parseFactor

-- parseFactor <* ((parseTs [AsteriskT] *> parseFactor) <|> (parseTs [DivisionT] *> parseFactor))

-- <exp> ::= <term> { ("+" | "-") <term> }
parseExpression :: Parser Expression
parseExpression = parseTerm

-- <statement> ::= "return" <exp> ";"
parseStatement :: Parser Statement
parseStatement =
  parseTs [KeywordT ReturnKW]
    *> (Return <$> parseExpression <* parseTs [SemiColonT])

-- <function> ::= "int" <id> "(" ")" "{" <statement> "}"
parseFuncDeclaration :: Parser FuncDeclaration
parseFuncDeclaration = do
  parseTs [KeywordT IntKW]
  identifier <- parseIdentifierLiteral
  parseTs [OpenParenthesisT, CloseParenthesisT, OpenBraceT]
  func <- Fun identifier <$> parseStatement
  parseTs [CloseBraceT]
  return func

-- <program> ::= <function>
parseProgram :: Parser Program
parseProgram = Program <$> parseFuncDeclaration
