module Parser
  ( parseAST,
    Program (..),
    FuncDeclaration (..),
    Statement (..),
    Expression (..),
    UnitaryOperator (..),
    BinaryOperator (..),
    BlockItem (..),
    Declaration (..),
  )
where

import Lexer
  ( Keyword (ElseKW, IfKW, IntKW, ReturnKW),
    Literal (IdentifierL, IntL),
    Token
      ( AndT,
        AssignmentT,
        AsteriskT,
        BangT,
        CloseBraceT,
        CloseParenthesisT,
        ColonT,
        DivisionT,
        GreaterThanOrEqualT,
        GreaterThanT,
        InequalityT,
        KeywordT,
        LessThanOrEqualT,
        LessThanT,
        LiteralT,
        LogicalEqualityT,
        MinusT,
        OpenBraceT,
        OpenParenthesisT,
        OrT,
        PlusT,
        QuestionMarkT,
        SemiColonT,
        TildeT
      ),
  )

data UnitaryOperator
  = Negation
  | LogicalNegation
  | BitwiseComplement
  deriving (Show, Eq)

data BinaryOperator
  = Addition
  | Subtraction
  | Multiplication
  | Division
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
  | UnOp UnitaryOperator Expression
  | BinOp BinaryOperator Expression Expression
  | Assignment String Expression
  | Variable String
  | ConditionalExpression Expression Expression Expression
  deriving (Show, Eq)

data Statement
  = Return Expression
  | Expression Expression
  | If Expression Statement (Maybe Statement)
  | Conditional Expression Statement (Maybe Statement)
  deriving (Show, Eq)

data Declaration = Declaration String (Maybe Expression) deriving (Show, Eq)

data BlockItem = State Statement | Declare Declaration deriving (Show, Eq)

data FuncDeclaration = Fun String [BlockItem] deriving (Show, Eq)

newtype Program = Program FuncDeclaration deriving (Show, Eq)

type Parser = [Token] -> Program

parseAST :: Parser
-- Program ::= FuncDeclaration
parseAST tokens = Program (parseFuncDeclaration tokens) -- `debug` show tokens

parseFuncDeclaration :: [Token] -> FuncDeclaration
-- FuncDeclaration ::= "int" Identifier "(" ")" "{" { BlockItem } "}"
parseFuncDeclaration
  ( _
      : LiteralT (IdentifierL funcName)
      : OpenParenthesisT
      : CloseParenthesisT
      : OpenBraceT
      : tokens
    ) = Fun funcName (parseBlockItems tokens)
parseFuncDeclaration _ = error "Invalid function declaration"

parseBlockItems :: [Token] -> [BlockItem]
parseBlockItems [] = []
parseBlockItems (CloseBraceT : _) = []
parseBlockItems tokens =
  blockItem : parseBlockItems rest -- `debug` show tokens
  where
    (blockItem, rest) = parseBlockItem tokens

parseBlockItem :: [Token] -> (BlockItem, [Token])
-- BlockItem ::= Statement | Declaration
parseBlockItem tokens@(KeywordT IntKW : _) =
  (Declare declaration, rest)
  where
    (declaration, rest) = parseDeclaration tokens
parseBlockItem tokens = (State statement, rest)
  where
    (statement, rest) = parseStatement tokens

parseDeclaration :: [Token] -> (Declaration, [Token])
-- Declaration ::= "int" Identifier [ "=" Expression ] ";"
parseDeclaration
  ( KeywordT IntKW
      : LiteralT (IdentifierL identifier)
      : AssignmentT
      : tokens
    ) =
    (Declaration identifier (Just expr), rest)
    where
      (expr, rest) = parseExpression tokens
parseDeclaration
  ( KeywordT IntKW
      : LiteralT (IdentifierL identifier)
      : SemiColonT
      : tokens
    ) =
    (Declaration identifier Nothing, tokens)
parseDeclaration tokens = error $ "Invalid syntax in declaration: " ++ show tokens

parseStatement :: [Token] -> (Statement, [Token])
-- Statement ::= "return" Expression ";"
--             | "int" Identifier [ "=" Expression ] ";"
--             | "if" "(" Expression ")" Statement [ "else" Statement ]
parseStatement
  ( KeywordT ReturnKW
      : tokens
    ) = (Return expr, rest)
    where
      (expr, rest) = parseExpression tokens
parseStatement (KeywordT IfKW : OpenParenthesisT : tokens) =
  (If expr statement elseStatement, rest)
  where
    (expr, restAfterExpr) = parseExpression tokens
    (statement, restAfterStatement) = parseStatement restAfterExpr
    (elseStatement, rest) = parseElseStatement restAfterStatement
parseStatement tokens =
  (Expression expr, rest)
  where
    (expr, rest) = parseExpression tokens

parseElseStatement :: [Token] -> (Maybe Statement, [Token])
parseElseStatement (KeywordT IfKW : OpenParenthesisT : tokens) =
  (Just (If expr statement elseStatement), rest)
  where
    (expr, restAfterExpr) = parseExpression tokens
    (statement, restAfterStatement) = parseStatement restAfterExpr
    (elseStatement, rest) = parseElseStatement restAfterStatement
parseElseStatement (KeywordT ElseKW : OpenBraceT : tokens) = (Just statement, rest)
  where
    (statement, rest) = parseStatement tokens
parseElseStatement (KeywordT ElseKW : tokens) = (Just statement, tokensAfter)
  where
    (statement, tokensAfter) = parseStatement tokens
parseElseStatement tokens = (Nothing, tokens)

parseExpression :: [Token] -> (Expression, [Token])
-- Expression ::= Identifier "=" Expression
--              | ConditionalExpression
parseExpression tokens =
  parseExpressionInternal term rest
  where
    (term, rest) = parseConditionalExpression tokens

parseExpressionInternal :: Expression -> [Token] -> (Expression, [Token])
parseExpressionInternal (Variable var) (AssignmentT : tokens) =
  parseExpressionInternal (Assignment var nextTerm) tokensAfterNextTerm
  where
    (nextTerm, tokensAfterNextTerm) = parseConditionalExpression tokens
parseExpressionInternal expr (SemiColonT : CloseBraceT : tokens) = (expr, tokens)
parseExpressionInternal expr (SemiColonT : tokens) = (expr, tokens)
parseExpressionInternal expr (CloseParenthesisT : tokens) = (expr, tokens)
parseExpressionInternal expr (ColonT : tokens) = (expr, tokens)
parseExpressionInternal expr tokens = error $ "Invalid syntax in expression: " ++ show expr ++ " " ++ show tokens

parseConditionalExpression :: [Token] -> (Expression, [Token])
-- ConditionalExpression ::= LogicalOrExp [ "?" Expression ":" ConditionalExpression ]
parseConditionalExpression tokens =
  parseConditionalExpressionInternal term rest
  where
    (term, rest) = parseLogicalOrExp tokens

parseConditionalExpressionInternal :: Expression -> [Token] -> (Expression, [Token])
parseConditionalExpressionInternal expr (QuestionMarkT : tokens) =
  parseConditionalExpressionInternal
    ( ConditionalExpression expr trueExpr falseExpr
    )
    tokensAfterFalseExpr
  where
    (trueExpr, tokensAfterTrueExpr) = parseExpression tokens
    (falseExpr, tokensAfterFalseExpr) = parseConditionalExpression tokensAfterTrueExpr
parseConditionalExpressionInternal expr tokens = (expr, tokens)

parseLogicalOrExp :: [Token] -> (Expression, [Token])
-- LogicalOrExp :== LogicalAndExp { "||" LogicalAndExp }
parseLogicalOrExp tokens =
  parseLogicalOrExpInternal term rest
  where
    (term, rest) = parseLogicalAndExp tokens

parseLogicalOrExpInternal :: Expression -> [Token] -> (Expression, [Token])
parseLogicalOrExpInternal expr tokens@(t : ts) =
  let (nextFactor, tokensAfterNextFactor) = parseLogicalAndExp ts
   in case t of
        OrT -> parseLogicalOrExpInternal (BinOp LogicalOr expr nextFactor) tokensAfterNextFactor
        _ -> (expr, tokens)
parseLogicalOrExpInternal _ [] = error "Invalid syntax in logical or expression"

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
parseFactor (LiteralT (IdentifierL identifier) : tokens) = (Variable identifier, tokens)
parseFactor (OpenParenthesisT : tokens) = parseExpression tokens
parseFactor (t : ts) =
  let (expr, rest) = parseFactor ts
   in case t of
        BangT -> (UnOp LogicalNegation expr, rest)
        TildeT -> (UnOp BitwiseComplement expr, rest)
        MinusT -> (UnOp Negation expr, rest)
        _ -> error $ "Invalid syntax in factor " ++ show (t : ts)
parseFactor tokens = error $ "Invalid syntax in factor" ++ show tokens