module Parser
  ( parseAST,
    Program (..),
    FuncDeclaration (..),
    Statement (..),
    Expression (..),
  )
where

import Lexer (Keyword (..), Literal (..), Token (..))

data Operator
  = Addition
  | Subtraction
  | Negation
  | Multiplication
  | Division
  | LogicalNegation
  | BitwiseComplement
  deriving (Show, Eq)

data Expression
  = Constant Integer
  | UnOp Operator Expression
  | BinOp Operator Expression Expression
  deriving (Show, Eq)

data Statement = Return Expression deriving (Show, Eq)

data FuncDeclaration = Fun String Statement deriving (Show, Eq)

data Program = Program FuncDeclaration deriving (Show, Eq)

type Parser = [Token] -> Program

parseAST :: Parser
parseAST tokens = Program (parseFuncDeclaration tokens)

parseFuncDeclaration :: [Token] -> FuncDeclaration
parseFuncDeclaration
  ( Literal (IdentifierL _)
      : Literal (IdentifierL funcName)
      : OpenParenthesis
      : CloseParenthesis
      : OpenBrace
      : tokens
    ) = Parser.Fun funcName (parseStatement tokens)
parseFuncDeclaration _ = error "Invalid function declaration"

parseStatement :: [Token] -> Statement
parseStatement (Keyword Lexer.Return : tokens) = Parser.Return (parseExpression tokens)
parseStatement _ = error "Invalid statement"

parseExpression :: [Token] -> Expression
parseExpression tokens =
  parseExpression' term rest
  where
    (term, rest) = parseTerm tokens

parseExpression' :: Expression -> [Token] -> Expression
parseExpression' expr (Lexer.Plus : tokens) =
  parseExpression' (BinOp Parser.Addition expr term) rest
  where
    (term, rest) = parseTerm tokens
parseExpression' expr (Lexer.Minus : tokens) =
  parseExpression' (BinOp Parser.Subtraction expr term) rest
  where
    (term, rest) = parseTerm tokens
parseExpression' expr (Lexer.SemiColon : _) = expr
parseExpression' expr [] = expr
parseExpression' _ _ = error "Invalid syntax in expression"

parseTerm :: [Token] -> (Expression, [Token])
parseTerm tokens =
  parseTerm' factor rest
  where
    (factor, rest) = parseFactor tokens

parseTerm' :: Expression -> [Token] -> (Expression, [Token])
parseTerm' expr (Lexer.Asterisk : tokens) =
  parseTerm' (BinOp Parser.Multiplication expr factor) rest
  where
    (factor, rest) = parseFactor tokens
parseTerm' expr tokens = (expr, tokens)

parseFactor :: [Token] -> (Expression, [Token])
parseFactor (Literal (IntL value) : tokens) = (Constant value, tokens)
parseFactor (Lexer.Bang : tokens) =
  (UnOp Parser.LogicalNegation expr, rest)
  where
    (expr, rest) = parseFactor tokens
parseFactor (Lexer.Tilde : tokens) =
  (UnOp Parser.BitwiseComplement expr, rest)
  where
    (expr, rest) = parseFactor tokens
parseFactor _ = error "Invalid syntax in factor"