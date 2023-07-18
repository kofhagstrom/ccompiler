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
parseLogicalAndExpInternal expr tokens@(t : ts) =
  let (nextFactor, tokensAfterNextFactor) = parseEqualityExp ts
   in case t of
        AndT -> parseLogicalAndExpInternal (BinOp LogicalAnd expr nextFactor) tokensAfterNextFactor
        _ -> (expr, tokens)
parseLogicalAndExpInternal _ [] = error "Invalid syntax in logical and expression"

parseEqualityExp :: [Token] -> (Expression, [Token])
-- EqualityExp :== RelationalExp { ("!=" | "==") RelationalExp }
parseEqualityExp tokens =
  parseEqualityExpInternal term rest
  where
    (term, rest) = parseRelationalExp tokens

parseEqualityExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseEqualityExpInternal expr tokens@(t : ts) =
  let (nextFactor, tokensAfterNextFactor) = parseRelationalExp ts
   in case t of
        LogicalEqualityT -> parseEqualityExpInternal (BinOp Equality expr nextFactor) tokensAfterNextFactor
        InequalityT -> parseEqualityExpInternal (BinOp Inequality expr nextFactor) tokensAfterNextFactor
        _ -> (expr, tokens)
parseEqualityExpInternal _ [] = error "Invalid syntax in equality expression"

parseRelationalExp :: [Token] -> (Expression, [Token])
-- RelationalExp :== AdditiveExp { ("<" | ">" | "<=" | ">=") AdditiveExp }
parseRelationalExp tokens =
  parseRelationalExpInternal term rest
  where
    (term, rest) = parseAdditiveExp tokens

parseRelationalExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseRelationalExpInternal expr tokens@(t : ts) =
  let (nextTerm, tokensAfterNextTerm) = parseAdditiveExp ts
   in case t of
        LessThanT ->
          parseRelationalExpInternal (BinOp LessThan expr nextTerm) tokensAfterNextTerm
        GreaterThanT ->
          parseRelationalExpInternal (BinOp GreaterThan expr nextTerm) tokensAfterNextTerm
        LessThanOrEqualT ->
          parseRelationalExpInternal (BinOp LessThanOrEqual expr nextTerm) tokensAfterNextTerm
        GreaterThanOrEqualT ->
          parseRelationalExpInternal (BinOp GreaterThanOrEqual expr nextTerm) tokensAfterNextTerm
        _ -> (expr, tokens)
parseRelationalExpInternal _ [] = error "Invalid syntax in relational expression"

parseAdditiveExp :: [Token] -> (Expression, [Token])
-- AdditiveExp :== Term { ("+" | "-") Term }
parseAdditiveExp tokens =
  parseAdditiveExpInternal term rest
  where
    (term, rest) = parseTerm tokens

parseAdditiveExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseAdditiveExpInternal expr tokens@(t : ts) =
  let (nextFactor, tokensAfterNextFactor) = parseTerm ts
   in case t of
        PlusT -> parseAdditiveExpInternal (BinOp Addition expr nextFactor) tokensAfterNextFactor
        MinusT -> parseAdditiveExpInternal (BinOp Subtraction expr nextFactor) tokensAfterNextFactor
        _ -> (expr, tokens)
parseAdditiveExpInternal _ [] = error "Invalid syntax in additive expression"

parseTerm :: [Token] -> (Expression, [Token])
-- Term ::= Term ( "*" | "/" ) Term | Factor
parseTerm tokens =
  parseTermInternal factor rest
  where
    (factor, rest) = parseFactor tokens

parseTermInternal :: Expression -> [Token] -> (Expression, [Token])
parseTermInternal expr tokens@(t : ts) =
  let (nextFactor, tokensAfterNextFactor) = parseFactor ts
   in case t of
        AsteriskT -> parseTermInternal (BinOp Multiplication expr nextFactor) tokensAfterNextFactor
        DivisionT -> parseTermInternal (BinOp Division expr nextFactor) tokensAfterNextFactor
        _ -> (expr, tokens)
parseTermInternal _ [] = error "Invalid syntax in term"

parseFactor :: [Token] -> (Expression, [Token])
-- Factor ::= "(" Expression ")" | UnOp Factor | Constant Integer
parseFactor (LiteralT (IntL value) : tokens) = (Constant value, tokens)
parseFactor (t : ts) =
  let (expr, rest) = parseFactor ts
   in case t of
        BangT -> (UnOp LogicalNegation expr, rest)
        TildeT -> (UnOp BitwiseComplement expr, rest)
        _ -> error "Invalid syntax in factor"
parseFactor _ = error "Invalid syntax in factor"