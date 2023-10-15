{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module ParserCombinator
  ( Parser (..),
    parseProgram,
    Program (..),
    FuncDeclaration (..),
    Statement (..),
    Expression (..),
    UnaryOperator (..),
    BinaryOperator (..),
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
  | Subtraction
  | LessThan
  | GreaterThan
  | LessThanOrEqual
  | GreaterThanOrEqual
  | Equality
  | Inequality
  | LogicalAnd
  | LogicalOr
  deriving (Show, Eq)

data Expression
  = BinOp BinaryOperator Expression Expression
  | UnOp UnaryOperator Expression
  | Constant Integer
  | Variable String
  deriving (Show, Eq)

newtype Statement = Return Expression deriving (Show, Eq)

data FuncDeclaration = Fun String Statement deriving (Show, Eq)

newtype Program = Program FuncDeclaration deriving (Show, Eq)

data ParseError
  = UnexpectedTokenError Token Token
  | UnexpectedError String

instance Show ParseError where
  show (UnexpectedTokenError t1 t2) = "Expected " ++ show t1 ++ ", got " ++ show t2 ++ ".\n"
  show (UnexpectedError msg) = msg ++ "\n"

newtype Parser a = Parser {runParser :: [Token] -> Either ([ParseError], [Token]) (a, [Token])}

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
  empty = Parser $ \_ -> Left ([], [])
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Right a -> Right a
      Left (e, _) -> case runParser p2 input of
        Right a' -> Right a'
        Left (e', ts') -> Left (e ++ e', ts')

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \input -> do
    (a, input') <- runParser p input
    runParser (f a) input'

parseT :: Token -> Parser Token
parseT t = Parser f
  where
    f (t' : ts) =
      if t' == t
        then Right (t', ts)
        else Left ([UnexpectedTokenError t t'], ts)
    f [] = Left ([UnexpectedError ("Expected " ++ show t ++ ", got nothing.")], [])

parseTs :: [Token] -> Parser [Token]
parseTs = traverse parseT

parseIdentifierLiteral :: Parser String
parseIdentifierLiteral = Parser $ \case
  (LiteralT (IdentifierL identifier) : ts) -> Right (identifier, ts)
  ts@(t : _) -> Left ([UnexpectedError ("Expected LiteralT (IdentifierL _), got " ++ show t)], ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

parseIntLiteral :: Parser Integer
parseIntLiteral = Parser $ \case
  (LiteralT (IntL value) : ts) -> Right (value, ts)
  ts@(t : _) -> Left ([UnexpectedError ("Expected LiteralT (IntL _), got " ++ show t)], ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

parseUnaryOperation :: Parser Expression
parseUnaryOperation =
  ( do
      parseTs [BangT]
      UnOp LogicalNegation <$> parseFactor
  )
    <|> ( do
            parseTs [MinusT]
            UnOp Negation <$> parseFactor
        )
    <|> ( do
            parseTs [TildeT]
            UnOp BitwiseComplement <$> parseFactor
        )

-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int>
parseFactor :: Parser Expression
parseFactor =
  ( do
      parseTs [OpenParenthesisT]
      expr <- parseExpression
      parseTs [CloseParenthesisT]
      return expr
  )
    <|> parseUnaryOperation
    <|> (Constant <$> parseIntLiteral)

-- <term> ::= <factor> { ("*" | "/") <factor> }
parseTerm :: Parser Expression
parseTerm = do
  e1 <- parseFactor
  loop e1
  where
    parseAdditionalFactor e = do
      t <- getNextToken
      e2 <- parseFactor
      case t of
        AsteriskT -> loop (BinOp Multiplication e e2)
        DivisionT -> loop (BinOp Division e e2)
        _ -> empty
    loop e = parseAdditionalFactor e <|> return e

getNextToken :: Parser Token
getNextToken = Parser $ \case
  (t : ts) -> Right (t, ts)
  _ -> Left ([UnexpectedError "Expected something, got nothing."], [])

-- <additive-exp> ::= <term> { ("+" | "-") <term> }
parseAdditiveExpression :: Parser Expression
parseAdditiveExpression = do
  e1 <- parseTerm
  loop e1
  where
    loop e = parseAdditionalTerm e <|> return e
    parseAdditionalTerm e = do
      t <- getNextToken
      e2 <- parseTerm
      case t of
        PlusT -> loop (BinOp Addition e e2)
        MinusT -> loop (BinOp Subtraction e e2)
        _ -> empty

-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
parseRelationalExpression :: Parser Expression
parseRelationalExpression = do
  e1 <- parseAdditiveExpression
  loop e1
  where
    loop e = parseAdditionalAdditiveExpression e <|> return e
    parseAdditionalAdditiveExpression e = do
      t <- getNextToken
      e2 <- parseAdditiveExpression
      case t of
        LessThanT -> loop (BinOp LessThan e e2)
        GreaterThanT -> loop (BinOp GreaterThan e e2)
        LessThanOrEqualT -> loop (BinOp LessThanOrEqual e e2)
        GreaterThanOrEqualT -> loop (BinOp GreaterThanOrEqual e e2)
        _ -> empty

-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
parseEqualityExpression :: Parser Expression
parseEqualityExpression = do
  e1 <- parseRelationalExpression
  loop e1
  where
    loop e = parseAdditionalRelationalExpression e <|> return e
    parseAdditionalRelationalExpression e = do
      t <- getNextToken
      e2 <- parseRelationalExpression
      case t of
        NotEqualT -> loop (BinOp Inequality e e2)
        LogicalEqualityT -> loop (BinOp Equality e e2)
        _ -> empty

-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
parseLogicalAndExpression :: Parser Expression
parseLogicalAndExpression = do
  e1 <- parseEqualityExpression
  loop e1
  where
    loop e = parseAdditionalEqualityExpression e <|> return e
    parseAdditionalEqualityExpression e = do
      t <- getNextToken
      e2 <- parseEqualityExpression
      case t of
        AndT -> loop (BinOp LogicalAnd e e2)
        _ -> empty

-- <exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
parseExpression :: Parser Expression
parseExpression = do
  e1 <- parseLogicalAndExpression
  loop e1
  where
    loop e = parseAdditionalLogicalAndExpression e <|> return e
    parseAdditionalLogicalAndExpression e = do
      t <- getNextToken
      e2 <- parseLogicalAndExpression
      case t of
        OrT -> loop (BinOp LogicalOr e e2)
        _ -> empty

-- <statement> ::= "return" <exp> ";"
parseStatement :: Parser Statement
parseStatement = do
  parseTs [KeywordT ReturnKW]
  expr <- Return <$> parseExpression
  parseTs [SemiColonT]
  return expr

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
