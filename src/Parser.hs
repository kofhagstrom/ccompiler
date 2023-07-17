module Parser
  ( parseAST,
    Program (..),
    FuncDeclaration (..),
    Statement (..),
    Expression (..),
  )
where

import Lexer
  ( Keyword (..),
    Literal (..),
    Token (..),
  )

data Operator
  = Addition
  | Subtraction
  | Negation
  | Multiplication
  | Division
  | LogicalNegation
  | BitwiseComplement
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
  = Constant Integer
  | UnOp Operator Expression
  | BinOp Operator Expression Expression
  deriving (Show, Eq)

newtype Statement = Return Expression deriving (Show, Eq)

data FuncDeclaration = Fun String Statement deriving (Show, Eq)

newtype Program = Program FuncDeclaration deriving (Show, Eq)

type Parser = [Token] -> Program

parseAST :: Parser
parseAST tokens = Program (parseFuncDeclaration tokens)

parseFuncDeclaration :: [Token] -> FuncDeclaration
parseFuncDeclaration
  ( LiteralT (IdentifierL _)
      : LiteralT (IdentifierL funcName)
      : OpenParenthesisT
      : CloseParenthesisT
      : OpenBraceT
      : tokens
    ) = Fun funcName (parseStatement tokens)
parseFuncDeclaration _ = error "Invalid function declaration"

parseStatement :: [Token] -> Statement
parseStatement (KeywordT Lexer.Return : tokens) = Parser.Return (parseExpression tokens)
parseStatement _ = error "Invalid statement"

parseExpression :: [Token] -> Expression
-- Expression ::= LogicalAndExp { "||" LogicalAndExp }
-- LogicalAndExp :== EqualityExp { "&&" EqualityExp }
-- EqualityExp :== RelationalExp { ("!=" | "==") RelationalExp }
-- RelationalExp :== AdditiveExp { ("<" | ">" | "<=" | ">=") AdditiveExp }
-- AdditiveExp :== Term { ("+" | "-") Term }
-- To parse an expression, we first parse a term.
parseExpression tokens =
  parseExpressionInternal term rest
  where
    -- (Parsing a term)
    (term, rest) = parseTerm tokens
    -- Then we parse addition or subtraction by combining this term
    -- with the next term (if any).
    parseExpressionInternal expr (PlusT : tokens) =
      parseExpressionInternal (BinOp Addition expr nextTerm) tokensAfterNextTerm
      where
        (nextTerm, tokensAfterNextTerm) = parseTerm tokens
    parseExpressionInternal expr (MinusT : tokens) =
      parseExpressionInternal (BinOp Subtraction expr nextTerm2) tokensAfterNextTerm2
      where
        (nextTerm2, tokensAfterNextTerm2) = parseTerm tokens
    -- If we see a semicolon, we stop parsing the expression.
    parseExpressionInternal expr (SemiColonT : _) = expr
    parseExpressionInternal _ _ = error "Invalid syntax in expression"

parseTerm :: [Token] -> (Expression, [Token])
-- Term ::= Term ( "*" | "/" ) Term | Factor
-- To parse a term, we first parse a factor.
parseTerm tokens =
  parseTermInternal factor rest
  where
    -- (Parsing a factor)
    (factor, rest) = parseFactor tokens
    -- Then, if we see a multiplication or division operator,
    -- we parse another term and combine the two terms into a single expression.
    parseTermInternal expr (AsteriskT : tokens) =
      parseTermInternal (BinOp Multiplication expr nextFactor1) tokensAfterNextFactor1
      where
        (nextFactor1, tokensAfterNextFactor1) = parseFactor tokens
    parseTermInternal expr (DivisionT : tokens) =
      parseTermInternal (BinOp Division expr nextFactor2) tokensAfterNextFactor2
      where
        (nextFactor2, tokensAfterNextFactor2) = parseFactor tokens
    parseTermInternal expr tokens = (expr, tokens)

parseFactor :: [Token] -> (Expression, [Token])
-- Factor ::= "(" Expression ")" | UnOp Factor | Constant Integer
-- To parse a factor, we simply match the first token against
-- integer literals, ! and ~.
parseFactor (LiteralT (IntL value) : tokens) = (Constant value, tokens)
parseFactor (BangT : tokens) =
  (UnOp LogicalNegation expr, rest)
  where
    (expr, rest) = parseFactor tokens
parseFactor (TildeT : tokens) =
  (UnOp BitwiseComplement expr, rest)
  where
    (expr, rest) = parseFactor tokens
parseFactor _ = error "Invalid syntax in factor"