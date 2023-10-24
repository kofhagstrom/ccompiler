{-# LANGUAGE LambdaCase #-}

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
    optional,
  )
import Data.Functor
import LexerCombinator
  ( Keyword (BreakKW, ContinueKW, DoKW, ElseKW, ForKW, IfKW, IntKW, ReturnKW, WhileKW),
    Literal (IdentifierL, IntL),
    Token (..),
  )
import Parser (Parser (..))

type ASTParser a = Parser [Token] [ParseError] a

instance Show ParseError where
  show (UnexpectedTokenError t1 t2) = "Expected " ++ show t1 ++ ", got " ++ show t2 ++ ".\n"
  show (UnexpectedError msg) = msg ++ "\n"

data ParseError
  = UnexpectedTokenError Token Token
  | UnexpectedError String

newtype Program = Program [FuncDeclaration] deriving (Show, Eq)

data FuncDeclaration = Fun String [String] (Maybe [BlockItem]) deriving (Show, Eq)

data BlockItem = State Statement | Declaration Declaration deriving (Show, Eq)

data Declaration = Declare String (Maybe Expression) deriving (Show, Eq)

data Statement
  = Return Expression
  | Expression (Maybe Expression)
  | Conditional Expression Statement (Maybe Statement)
  | Compound [BlockItem]
  | For (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement
  | ForDecl Declaration (Maybe Expression) (Maybe Expression) Statement
  | While Expression Statement
  | Do Statement Expression
  | Break
  | Continue
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
parseProgram = Program <$> many parseFuncDeclaration

-- <function> ::= "int" <id> "(" [ "int" <id> { "," "int" <id> } ] ")" ( "{" { <block-item> } "}" | ";" )
parseFuncDeclaration :: ASTParser FuncDeclaration
parseFuncDeclaration = do
  _ <- parseTokens [KeywordT IntKW]
  identifier <- parseIdentifierLiteral
  _ <- parseTokens [OpenParenthesisT]
  firstArg <- optional (parseTokens [KeywordT IntKW] *> parseIdentifierLiteral)
  args <- many (parseTokens [CommaT, KeywordT IntKW] *> parseIdentifierLiteral)
  _ <- parseTokens [CloseParenthesisT, OpenBraceT]
  func <- case firstArg of
    Nothing -> Fun identifier args <$> optional (many parseBlockItem)
    Just arg -> Fun identifier (arg : args) <$> optional (many parseBlockItem)
  _ <- parseTokens [CloseBraceT]
  return func

-- <block-item> ::= <statement> | <declaration>
parseBlockItem :: ASTParser BlockItem
parseBlockItem = State <$> parseStatement <|> Declaration <$> parseDeclaration

-- <declaration> ::= "int" <id> [ = <exp> ] ";"
parseDeclaration :: ASTParser Declaration
parseDeclaration =
  ( do
      identifier <- parseDeclIdentifier
      _ <- parseTokens [AssignmentT]
      expr <- parseExpression
      _ <- parseTokens [SemiColonT]
      return (identifier (Just expr))
  )
    <|> ( do
            identifier <- parseDeclIdentifier
            _ <- parseTokens [SemiColonT]
            return (identifier Nothing)
        )
  where
    parseDeclIdentifier = parseTokens [KeywordT IntKW] *> (Declare <$> parseIdentifierLiteral)

-- <statement> ::= "return" <exp> ";"
--                | <exp-option> ";"
--                | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
--                | "{" { <block-item> } "}"
--                | "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
--                | "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
--                | "do" <statement> "while" "(" <exp> ")" ";"
--                | "break" ";"
--                | "continue" ";"
parseStatement :: ASTParser Statement
parseStatement =
  parseReturn
    <|> parseWhile
    <|> parseDo
    <|> parseBreak
    <|> parseContinue
    <|> parseCompound
    <|> parseFor
    <|> parseForDecl
    <|> parseExpr
    <|> parseIf
  where
    -- "if" "(" <exp> ")" <statement> [ "else" <statement> ]
    parseIf =
      do
        ifStmt <- parseConditional
        elseStmt <- optional (parseTokens [KeywordT ElseKW] *> parseStatement)
        return (ifStmt elseStmt)
      where
        parseConditional =
          Conditional
            <$> ( parseTokens [KeywordT IfKW, OpenParenthesisT]
                    *> parseExpression
                    <* parseTokens [CloseParenthesisT]
                )
            <*> parseStatement
    -- <exp-option> ";"
    parseExpr = (optional parseExpression <* parseTokens [SemiColonT]) <&> Expression
    -- "return" <exp> ";"
    parseReturn =
      parseTokens [KeywordT ReturnKW]
        *> (Return <$> parseExpression)
        <* parseTokens [SemiColonT]
    -- "while" "(" <exp> ")" <statement>
    parseWhile = do
      _ <- parseTokens [KeywordT WhileKW, OpenParenthesisT]
      expr <- parseExpression
      _ <- parseTokens [CloseParenthesisT]
      While expr <$> parseStatement
    -- "do" <statement> "while" "(" <exp> ")" ";"
    parseDo = do
      _ <- parseTokens [KeywordT DoKW, OpenParenthesisT]
      stmt <- parseStatement
      _ <- parseTokens [CloseParenthesisT]
      Do stmt <$> parseExpression
    parseBreak = parseTokens [KeywordT BreakKW, SemiColonT] $> Break
    parseContinue = parseTokens [KeywordT ContinueKW, SemiColonT] $> Continue
    parseCompound =
      parseTokens [OpenBraceT]
        *> (Compound <$> many parseBlockItem)
        <* parseTokens [CloseBraceT]
    -- "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
    parseFor =
      do
        _ <- parseTokens [KeywordT ForKW, OpenParenthesisT]
        expr <- optional (parseExpression <* parseTokens [SemiColonT])
        expr' <- optional (parseExpression <* parseTokens [SemiColonT])
        expr'' <- optional (parseExpression <* parseTokens [SemiColonT])
        _ <- parseTokens [SemiColonT, CloseParenthesisT]
        For expr expr' expr'' <$> parseStatement
    -- "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
    parseForDecl =
      do
          _ <- parseTokens [KeywordT ForKW, OpenParenthesisT]
          decl <- parseDeclaration
          expr <- optional (parseExpression <* parseTokens [SemiColonT])
          expr' <- optional parseExpression
          _ <- parseTokens [CloseParenthesisT]
          ForDecl decl expr expr' <$> parseStatement

-- <exp> ::= <id> "=" <exp> | <logical-or-exp>
parseExpression :: ASTParser Expression
parseExpression =
  Assign <$> (parseIdentifierLiteral <* parseTokens [AssignmentT]) <*> parseExpression
    <|> parseConditionalExpression

-- <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
parseConditionalExpression :: ASTParser Expression
parseConditionalExpression =
  ( do
      expr <- parseLogicalOrExpression
      _ <- parseTokens [QuestionMarkT]
      expr' <- parseExpression
      _ <- parseTokens [ColonT]
      ConditionalExpression expr expr' <$> parseConditionalExpression
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
  parseTokens [OpenParenthesisT]
    *> parseExpression
    <* parseTokens [CloseParenthesisT]
    <|> ( do
            t <- parseOneOfTheseTokens [BangT, MinusT, TildeT]
            case t of
              BangT -> UnaryOperator LogicalNegation <$> parseFactor
              MinusT -> UnaryOperator Negation <$> parseFactor
              TildeT -> UnaryOperator BitwiseComplement <$> parseFactor
              _ -> empty
        )
    <|> Constant <$> parseIntLiteral
    <|> Variable <$> parseIdentifierLiteral

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

parseOneOfTheseTokens :: [Token] -> ASTParser Token
parseOneOfTheseTokens =
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
