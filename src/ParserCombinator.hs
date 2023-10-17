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
    BlockItem (..),
    Declaration (..),
  )
where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    many,
  )
import LexerCombinator
  ( Keyword (ElseKW, IfKW, IntKW, ReturnKW),
    Literal (IdentifierL, IntL),
    Token (..),
  )
import Parser

type ASTParser a = Parser Token ParseError a

instance Show ParseError where
  show (UnexpectedTokenError t1 t2) = "Expected " ++ show t1 ++ ", got " ++ show t2 ++ ".\n"
  show (UnexpectedError msg) = msg ++ "\n"

data ParseError
  = UnexpectedTokenError Token Token
  | UnexpectedError String

newtype Program = Program FuncDeclaration deriving (Show, Eq)

data FuncDeclaration = Fun String [BlockItem] deriving (Show, Eq)

data BlockItem = State Statement | Declaration Declaration deriving (Show, Eq)

data Declaration = Declare String (Maybe Expression) deriving (Show, Eq)

data Statement
  = Return Expression
  | Expression Expression
  | Conditional Expression Statement (Maybe Statement)
  deriving (Show, Eq)

data Expression
  = BinaryOperator BinaryOperator Expression Expression
  | UnaryOperator UnaryOperator Expression
  | Constant Integer
  | Variable String
  | Assign String Expression
  | ConditionalExpression Expression Expression Expression
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

data UnaryOperator
  = Negation
  | LogicalNegation
  | BitwiseComplement
  deriving (Show, Eq)

-- <program> ::= <function>
parseProgram :: ASTParser Program
parseProgram = Program <$> parseFuncDeclaration

-- <function> ::= "int" <id> "(" ")" "{" { <block-item> } "}"
parseFuncDeclaration :: ASTParser FuncDeclaration
parseFuncDeclaration = do
  parseTokens [KeywordT IntKW]
  identifier <- parseIdentifierLiteral
  parseTokens [OpenParenthesisT, CloseParenthesisT, OpenBraceT]
  func <- Fun identifier <$> many parseBlockItem
  parseTokens [CloseBraceT]
  return func

-- <block-item> ::= <statement> | <declaration>
parseBlockItem :: ASTParser BlockItem
parseBlockItem = (State <$> parseStatement) <|> (Declaration <$> parseDeclaration)

-- <declaration> ::= "int" <id> [ = <exp> ] ";"
parseDeclaration :: ASTParser Declaration
parseDeclaration =
  ( do
      parseTokens [KeywordT IntKW]
      identifier <- Declare <$> parseIdentifierLiteral
      parseTokens [AssignmentT]
      expr <- parseExpression
      parseTokens [SemiColonT]
      return (identifier (Just expr))
  )
    <|> ( do
            parseTokens [KeywordT IntKW]
            identifier <- Declare <$> parseIdentifierLiteral
            parseTokens [SemiColonT]
            return (identifier Nothing)
        )

-- <statement> ::= "return" <exp> ";"
--                | <exp> ";"
--                | "int" <id> [ = <exp>] ";"
--                | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
parseStatement :: ASTParser Statement
parseStatement =
  ( do
      parseTokens [KeywordT ReturnKW]
      expr <- Return <$> parseExpression
      parseTokens [SemiColonT]
      return expr
  )
    <|> ( do
            expr <- Expression <$> parseExpression
            parseTokens [SemiColonT]
            return expr
        )
    <|> ( do
            parseTokens [KeywordT IfKW, OpenParenthesisT]
            expr <- parseExpression
            parseTokens [CloseParenthesisT]
            stmt <- parseStatement
            parseTokens [KeywordT ElseKW]
            stmt' <- parseStatement
            return (Conditional expr stmt (Just stmt'))
        )
    <|> ( do
            parseTokens [KeywordT IfKW, OpenParenthesisT]
            expr <- parseExpression
            parseTokens [CloseParenthesisT]
            stmt <- parseStatement
            parseTokens [KeywordT ElseKW]
            parseTokens [OpenBraceT]
            stmt' <- parseStatement
            parseTokens [CloseBraceT]
            return (Conditional expr stmt (Just stmt'))
        )
    <|> ( do
            parseTokens [KeywordT IfKW, OpenParenthesisT]
            expr <- parseExpression
            parseTokens [CloseParenthesisT]
            stmt <- parseStatement
            return (Conditional expr stmt Nothing)
        )

-- <exp> ::= <id> "=" <exp> | <logical-or-exp>
parseExpression :: ASTParser Expression
parseExpression =
  ( do
      identifier <- parseIdentifierLiteral
      parseTokens [AssignmentT]
      expr <- parseExpression
      return (Assign identifier expr)
  )
    <|> parseConditionalExpression

-- <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
parseConditionalExpression :: ASTParser Expression
parseConditionalExpression =
  ( do
      expr <- parseLogicalOrExpression
      parseTokens [QuestionMarkT]
      expr' <- parseExpression
      parseTokens [ColonT]
      expr'' <- parseConditionalExpression
      return (ConditionalExpression expr expr' expr'')
  )
    <|> parseLogicalOrExpression

-- <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
parseLogicalOrExpression :: ASTParser Expression
parseLogicalOrExpression = do
  e1 <- parseLogicalAndExpression
  loop e1
  where
    loop e =
      ( do
          t <- getNextToken
          e2 <- parseLogicalAndExpression
          case t of
            OrT -> loop (BinaryOperator LogicalOr e e2)
            _ -> empty
      )
        <|> return e

-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
parseLogicalAndExpression :: ASTParser Expression
parseLogicalAndExpression = do
  e1 <- parseEqualityExpression
  loop e1
  where
    loop e =
      ( do
          t <- getNextToken
          e2 <- parseEqualityExpression
          case t of
            AndT -> loop (BinaryOperator LogicalAnd e e2)
            _ -> empty
      )
        <|> return e

-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
parseEqualityExpression :: ASTParser Expression
parseEqualityExpression = do
  e1 <- parseRelationalExpression
  loop e1
  where
    loop e =
      ( do
          t <- getNextToken
          e2 <- parseRelationalExpression
          case t of
            NotEqualT -> loop (BinaryOperator Inequality e e2)
            LogicalEqualityT -> loop (BinaryOperator Equality e e2)
            _ -> empty
      )
        <|> return e

-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
parseRelationalExpression :: ASTParser Expression
parseRelationalExpression = do
  e1 <- parseAdditiveExpression
  loop e1
  where
    loop e =
      ( do
          t <- getNextToken
          e2 <- parseAdditiveExpression
          case t of
            LessThanT -> loop (BinaryOperator LessThan e e2)
            GreaterThanT -> loop (BinaryOperator GreaterThan e e2)
            LessThanOrEqualT -> loop (BinaryOperator LessThanOrEqual e e2)
            GreaterThanOrEqualT -> loop (BinaryOperator GreaterThanOrEqual e e2)
            _ -> empty
      )
        <|> return e

-- <additive-exp> ::= <term> { ("+" | "-") <term> }
parseAdditiveExpression :: ASTParser Expression
parseAdditiveExpression = do
  e1 <- parseTerm
  loop e1
  where
    loop e =
      ( do
          t <- getNextToken
          e2 <- parseTerm
          case t of
            PlusT -> loop (BinaryOperator Addition e e2)
            MinusT -> loop (BinaryOperator Subtraction e e2)
            _ -> empty
      )
        <|> return e

-- <term> ::= <factor> { ("*" | "/") <factor> }
parseTerm :: ASTParser Expression
parseTerm = do
  e1 <- parseFactor
  loop e1
  where
    loop e =
      ( do
          t <- getNextToken
          e2 <- parseFactor
          case t of
            AsteriskT -> loop (BinaryOperator Multiplication e e2)
            DivisionT -> loop (BinaryOperator Division e e2)
            _ -> empty
      )
        <|> return e

-- <factor> ::= "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
parseFactor :: ASTParser Expression
parseFactor =
  ( do
      parseTokens [OpenParenthesisT]
      expr <- parseExpression
      parseTokens [CloseParenthesisT]
      return expr
  )
    <|> parseUnaryOperation
    <|> (Constant <$> parseIntLiteral)
    <|> (Variable <$> parseIdentifierLiteral)

parseUnaryOperation :: ASTParser Expression
parseUnaryOperation = do
  t <- tryParseTokens [BangT, MinusT, TildeT]
  case t of
    BangT -> UnaryOperator LogicalNegation <$> parseFactor
    MinusT -> UnaryOperator Negation <$> parseFactor
    TildeT -> UnaryOperator BitwiseComplement <$> parseFactor
    _ -> empty

parseIntLiteral :: ASTParser Integer
parseIntLiteral = Parser $ \case
  (LiteralT (IntL value) : ts) -> Right (value, ts)
  ts@(t : _) -> Left ([UnexpectedError ("Expected LiteralT (IntL _), got " ++ show t)], ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

parseIdentifierLiteral :: ASTParser String
parseIdentifierLiteral = Parser $ \case
  (LiteralT (IdentifierL identifier) : ts) -> Right (identifier, ts)
  ts@(t : _) -> Left ([UnexpectedError ("Expected LiteralT (IdentifierL _), got " ++ show t)], ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

parseTokens :: [Token] -> ASTParser [Token]
parseTokens = traverse parseToken

tryParseTokens :: [Token] -> ASTParser Token
tryParseTokens =
  foldr
    ((<|>) . parseToken)
    (Parser $ \inp -> Left ([UnexpectedError "Tried to parse on empty input"], inp))

parseToken :: Token -> ASTParser Token
parseToken t = Parser f
  where
    f (t' : ts) =
      if t' == t
        then Right (t', ts)
        else Left ([UnexpectedTokenError t t'], ts)
    f [] = Left ([UnexpectedError ("Expected " ++ show t ++ ", got nothing.")], [])

getNextToken :: ASTParser Token
getNextToken = Parser $ \case
  (t : ts) -> Right (t, ts)
  _ -> Left ([UnexpectedError "Expected something, got nothing."], [])
