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
import Data.Functor (($>))
import Data.Maybe (maybeToList)
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
  | FunCall String [Expression]
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
  identifier <- parseTokens [KeywordT IntKW] *> parseIdentifierLiteral
  firstArg <- parseTokens [OpenParenthesisT] *> optional (parseTokens [KeywordT IntKW] *> parseIdentifierLiteral)
  args <- many (parseTokens [CommaT, KeywordT IntKW] *> parseIdentifierLiteral) <* parseTokens [CloseParenthesisT, OpenBraceT]
  Fun identifier (maybeToList firstArg ++ args) <$> (optional (many parseBlockItem) <* parseTokens [CloseBraceT])

-- <block-item> ::= <statement> | <declaration>
parseBlockItem :: ASTParser BlockItem
parseBlockItem = State <$> parseStatement <|> Declaration <$> parseDeclaration

-- <declaration> ::= "int" <id> [ = <exp> ] ";"
parseDeclaration :: ASTParser Declaration
parseDeclaration =
  do
    identifier <- parseTokens [KeywordT IntKW] *> (Declare <$> parseIdentifierLiteral)
    expr <- optional (parseTokens [AssignmentT] *> parseExpression) <* parseTokens [SemiColonT]
    return (identifier expr)

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
    <|> parseForDeclaration
    <|> parseOptionalExpression
    <|> parseConditional
  where
    -- "if" "(" <exp> ")" <statement> [ "else" <statement> ]
    parseConditional =
      Conditional
        <$> ( parseTokens [KeywordT IfKW, OpenParenthesisT]
                *> parseExpression
                <* parseTokens [CloseParenthesisT]
            )
        <*> parseStatement
        <*> optional (parseTokens [KeywordT ElseKW] *> parseStatement)
    -- <exp-option> ";"
    parseOptionalExpression = Expression <$> (optional parseExpression <* parseTokens [SemiColonT])
    -- "return" <exp> ";"
    parseReturn =
      Return
        <$> ( parseTokens [KeywordT ReturnKW]
                *> parseExpression
                <* parseTokens [SemiColonT]
            )
    -- "while" "(" <exp> ")" <statement>
    parseWhile =
      While
        <$> (parseTokens [KeywordT WhileKW, OpenParenthesisT] *> parseExpression <* parseTokens [CloseParenthesisT])
        <*> parseStatement
    -- "do" <statement> "while" "(" <exp> ")" ";"
    parseDo =
      Do
        <$> (parseTokens [KeywordT DoKW] *> parseStatement <* parseTokens [KeywordT WhileKW, OpenParenthesisT])
        <*> (parseExpression <* parseTokens [CloseParenthesisT, SemiColonT])
    -- "{" { <block-item> } "}"
    parseCompound =
      Compound
        <$> ( parseTokens [OpenBraceT]
                *> many parseBlockItem
                <* parseTokens [CloseBraceT]
            )
    -- "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
    parseFor =
      do
        (expr, expr', expr'') <-
          parseTokens [KeywordT ForKW, OpenParenthesisT]
            *> ( (,,)
                   <$> parseOptionalExpression'
                   <*> parseOptionalExpression'
                   <*> parseOptionalExpression'
               )
            <* parseTokens [SemiColonT, CloseParenthesisT]
        For expr expr' expr'' <$> parseStatement
      where
        parseOptionalExpression' = optional (parseExpression <* parseTokens [SemiColonT])
    -- "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
    parseForDeclaration =
      do
        decl <- parseTokens [KeywordT ForKW, OpenParenthesisT] *> parseDeclaration
        (expr, expr') <-
          (,)
            <$> optional (parseExpression <* parseTokens [SemiColonT])
            <*> (optional parseExpression <* parseTokens [CloseParenthesisT])
        ForDecl decl expr expr' <$> parseStatement
    -- "break" ";"
    parseBreak = parseTokens [KeywordT BreakKW, SemiColonT] $> Break
    -- "continue" ";"
    parseContinue = parseTokens [KeywordT ContinueKW, SemiColonT] $> Continue

-- <exp> ::= <id> "=" <exp> | <conditional-exp>
parseExpression :: ASTParser Expression
parseExpression =
  Assign <$> (parseIdentifierLiteral <* parseTokens [AssignmentT]) <*> parseExpression
    <|> parseConditionalExpression

-- <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
parseConditionalExpression :: ASTParser Expression
parseConditionalExpression =
  ( do
      (expr, expr') <- (,) <$> parseLogicalOrExpression <* parseTokens [QuestionMarkT] <*> (parseExpression <* parseTokens [ColonT])
      ConditionalExpression expr expr' <$> parseConditionalExpression
  )
    <|> parseLogicalOrExpression

-- <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
parseLogicalOrExpression :: ASTParser Expression
parseLogicalOrExpression =
  loop
    parseLogicalAndExpression
    ( \t e e' ->
        case t of
          OrT -> Just (BinaryOperator LogicalOr e e')
          _ -> Nothing
    )

-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
parseLogicalAndExpression :: ASTParser Expression
parseLogicalAndExpression =
  loop
    parseEqualityExpression
    ( \t e e' ->
        case t of
          AndT -> Just (BinaryOperator LogicalAnd e e')
          _ -> Nothing
    )

-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
parseEqualityExpression :: ASTParser Expression
parseEqualityExpression =
  loop
    parseRelationalExpression
    ( \t e e' ->
        case t of
          NotEqualT -> Just (BinaryOperator Inequality e e')
          LogicalEqualityT -> Just (BinaryOperator Equality e e')
          _ -> Nothing
    )

-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
parseRelationalExpression :: ASTParser Expression
parseRelationalExpression =
  loop
    parseAdditiveExpression
    ( \t e e' ->
        case t of
          LessThanT -> Just (BinaryOperator LessThan e e')
          GreaterThanT -> Just (BinaryOperator GreaterThan e e')
          LessThanOrEqualT -> Just (BinaryOperator LessThanOrEqual e e')
          GreaterThanOrEqualT -> Just (BinaryOperator GreaterThanOrEqual e e')
          _ -> Nothing
    )

-- <additive-exp> ::= <term> { ("+" | "-") <term> }
parseAdditiveExpression :: ASTParser Expression
parseAdditiveExpression =
  loop
    parseTerm
    ( \t e e' ->
        case t of
          PlusT -> Just (BinaryOperator Addition e e')
          MinusT -> Just (BinaryOperator Subtraction e e')
          _ -> Nothing
    )

-- <term> ::= <factor> { ("*" | "/") <factor> }
parseTerm :: ASTParser Expression
parseTerm =
  loop
    parseFactor
    ( \t e e' ->
        case t of
          AsteriskT -> Just (BinaryOperator Multiplication e e')
          DivisionT -> Just (BinaryOperator Division e e')
          _ -> Nothing
    )

-- <factor> ::= <function-call> | "(" <exp> ")" | <unary_op> <factor> | <int> | <id>
parseFactor :: ASTParser Expression
parseFactor =
  parseFunctionCall
    <|> parseTokens [OpenParenthesisT] *> parseExpression <* parseTokens [CloseParenthesisT]
    <|> ( parseOneOfTheseTokens [BangT, MinusT, TildeT]
            >>= \case
              BangT -> UnaryOperator LogicalNegation <$> parseFactor
              MinusT -> UnaryOperator Negation <$> parseFactor
              TildeT -> UnaryOperator BitwiseComplement <$> parseFactor
              _ -> empty
        )
    <|> Constant <$> parseIntLiteral
    <|> Variable <$> parseIdentifierLiteral

-- <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"
parseFunctionCall :: ASTParser Expression
parseFunctionCall = do
  identifier <- parseIdentifierLiteral
  _ <- parseTokens [OpenParenthesisT]
  firstArg <- optional parseExpression
  args <- many (parseTokens [CommaT] *> parseExpression)
  _ <- parseTokens [CloseParenthesisT]
  return (FunCall identifier (maybeToList firstArg ++ args))

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
    f ts@(t' : rest) =
      if t' == t
        then Right (t', rest)
        else Left ([UnexpectedTokenError t t'], ts)
    f [] = Left ([UnexpectedError ("Expected " ++ show t ++ ", got nothing.")], [])

getNextToken :: ASTParser Token
getNextToken = Parser $ \case
  (t : ts) -> Right (t, ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

-- parses a grammar of type <A> ::= <B> { ("a" | "b" | ... ) <B> }
-- parserB is a parser which parses Bs, and tokenToOperator is a function which matches tokens to operators
loop :: Parser [Token] [ParseError] b -> (Token -> b -> b -> Maybe b) -> Parser [Token] [ParseError] b
loop parserB tokenToOperator = parserB >>= loop'
  where
    loop' e =
      ( do
          t <- getNextToken
          p <- parserB
          maybe empty loop' (tokenToOperator t e p)
      )
        <|> return e
