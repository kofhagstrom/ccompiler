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
-- Expression ::= Expression BinOp Expression | Term
-- To parse an expression, we first parse a term.
parseExpression tokens =
  parseExpressionInternal term rest
  where
    -- (Parsing a term)
    (term, rest) = parseTerm tokens
    -- Then we parse addition or subtraction by combining this term
    -- with the next term (if any).
    parseExpressionInternal expr (Lexer.Plus : tokens) =
      parseExpressionInternal (BinOp Parser.Addition expr nextTerm) tokensAfterNextTerm
      where
        (nextTerm, tokensAfterNextTerm) = parseTerm tokens
    parseExpressionInternal expr (Lexer.Minus : tokens) =
      parseExpressionInternal (BinOp Parser.Subtraction expr nextTerm2) tokensAfterNextTerm2
      where
        (nextTerm2, tokensAfterNextTerm2) = parseTerm tokens
    -- If we see a semicolon, we stop parsing the expression.
    parseExpressionInternal expr (Lexer.SemiColon : _) = expr
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
    parseTermInternal expr (Lexer.Asterisk : tokens) =
      parseTermInternal (BinOp Parser.Multiplication expr nextFactor) tokensAfterNextFactor
      where
        (nextFactor, tokensAfterNextFactor) = parseFactor tokens
    parseTermInternal expr tokens = (expr, tokens)

parseFactor :: [Token] -> (Expression, [Token])
-- Factor ::= "(" Expression ")" | UnOp Factor | Constant Integer
-- To parse a factor, we simply match the first token against
-- integer literals, ! and ~.
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