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
parseExpression tokens =
  parseExpressionInternal term rest
  where
    (term, rest) = parseLogicalAndExp tokens

parseExpressionInternal :: Expression -> [Token] -> Expression
parseExpressionInternal expr (OrT : tokens) =
  parseExpressionInternal (BinOp LogicalOr expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseLogicalAndExp tokens
parseExpressionInternal expr (SemiColonT : _) = expr
parseExpressionInternal _ _ = error "Invalid syntax in expression"

parseLogicalAndExp :: [Token] -> (Expression, [Token])
-- LogicalAndExp :== EqualityExp { "&&" EqualityExp }
parseLogicalAndExp tokens =
  parseLogicalAndExpInternal term rest
  where
    (term, rest) = parseEqualityExp tokens

parseLogicalAndExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseLogicalAndExpInternal expr (AndT : tokens) =
  parseLogicalAndExpInternal (BinOp LogicalAnd expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseEqualityExp tokens
parseLogicalAndExpInternal expr tokens = (expr, tokens)

parseEqualityExp :: [Token] -> (Expression, [Token])
-- EqualityExp :== RelationalExp { ("!=" | "==") RelationalExp }
parseEqualityExp tokens =
  parseEqualityExpInternal term rest
  where
    (term, rest) = parseRelationalExp tokens

parseEqualityExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseEqualityExpInternal expr (LogicalEqualityT : tokens) =
  parseEqualityExpInternal (BinOp Equality expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseRelationalExp tokens
parseEqualityExpInternal expr (InequalityT : tokens) =
  parseEqualityExpInternal (BinOp Inequality expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseRelationalExp tokens
parseEqualityExpInternal expr tokens = (expr, tokens)

parseRelationalExp :: [Token] -> (Expression, [Token])
-- RelationalExp :== AdditiveExp { ("<" | ">" | "<=" | ">=") AdditiveExp }
parseRelationalExp tokens =
  parseRelationalExpInternal term rest
  where
    (term, rest) = parseAdditiveExp tokens

parseRelationalExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseRelationalExpInternal expr (LessThanT : tokens) =
  parseRelationalExpInternal (BinOp LessThan expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseAdditiveExp tokens
parseRelationalExpInternal expr (GreaterThanT : tokens) =
  parseRelationalExpInternal (BinOp GreaterThan expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseAdditiveExp tokens
parseRelationalExpInternal expr (LessThanOrEqualT : tokens) =
  parseRelationalExpInternal (BinOp LessThanOrEqual expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseAdditiveExp tokens
parseRelationalExpInternal expr (GreaterThanOrEqualT : tokens) =
  parseRelationalExpInternal (BinOp GreaterThanOrEqual expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseAdditiveExp tokens
parseRelationalExpInternal expr tokens = (expr, tokens)

parseAdditiveExp :: [Token] -> (Expression, [Token])
-- AdditiveExp :== Term { ("+" | "-") Term }
parseAdditiveExp tokens =
  parseAdditiveExpInternal term rest
  where
    (term, rest) = parseTerm tokens

parseAdditiveExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseAdditiveExpInternal expr (PlusT : tokens) =
  parseAdditiveExpInternal (BinOp Addition expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseTerm tokens
parseAdditiveExpInternal expr (MinusT : tokens) =
  parseAdditiveExpInternal (BinOp Subtraction expr nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseTerm tokens
parseAdditiveExpInternal expr tokens = (expr, tokens)

parseTerm :: [Token] -> (Expression, [Token])
-- Term ::= Term ( "*" | "/" ) Term | Factor
parseTerm tokens =
  parseTermInternal factor rest
  where
    (factor, rest) = parseFactor tokens

parseTermInternal :: Expression -> [Token] -> (Expression, [Token])
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