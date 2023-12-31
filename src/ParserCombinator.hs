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
    TopLevelItem (..),
    ParseError (..),
    Constant (..),
    CType (..),
  )
where

import Control.Applicative
  ( Alternative
      ( empty,
        (<|>)
      ),
    many,
    optional,
  )
import Data.Functor (($>))
import Data.Maybe (maybeToList)
import LexerCombinator
  ( Keyword
      ( BreakKW,
        ContinueKW,
        DoKW,
        ElseKW,
        ForKW,
        IfKW,
        IntKW,
        ReturnKW,
        StringKW,
        WhileKW
      ),
    Literal
      ( IdentifierL,
        IntL,
        StringL
      ),
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

newtype Program = Program [TopLevelItem] deriving (Show, Eq)

data TopLevelItem = F FuncDeclaration | D Declaration deriving (Show, Eq)

data CType = CInt | CString deriving (Show, Eq)

data FuncDeclaration = Fun CType String [String] (Maybe [BlockItem]) deriving (Show, Eq)

data BlockItem = State Statement | Declaration Declaration deriving (Show, Eq)

data Declaration = Declare CType String (Maybe Expression) deriving (Show, Eq)

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
  | Constant Constant
  | Variable String
  | Assign String Expression
  | ConditionalExpression Expression Expression Expression
  | FunCall String [Expression]
  deriving (Show, Eq)

data Constant
  = ConstantInt Integer
  | ConstantString String
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
parseProgram = Program <$> many (F <$> parseFuncDeclaration <|> D <$> parseDeclaration)

parseType :: ASTParser CType
parseType = do
  t <- parseOneOfTheseTokens [KeywordT IntKW, KeywordT StringKW]
  case t of
    KeywordT IntKW -> return CInt
    KeywordT StringKW -> return CString
    _ -> empty

-- <function> ::= ( "int" | "string" | "bool" ) <id> "(" [ "int" <id> { "," "int" <id> } ] ")" ( "{" { <block-item> } "}" | ";" )
parseFuncDeclaration :: ASTParser FuncDeclaration
parseFuncDeclaration = do
  returnType <- parseType
  identifier <- parseIdentifierLiteral
  firstArg <- parseTokens [OpenParenthesisT] *> optional (parseTokens [KeywordT IntKW] *> parseIdentifierLiteral)
  args <- many (parseTokens [CommaT, KeywordT IntKW] *> parseIdentifierLiteral) <* parseTokens [CloseParenthesisT, OpenBraceT]
  Fun returnType identifier (maybeToList firstArg ++ args) <$> (optional (many parseBlockItem) <* parseTokens [CloseBraceT])

-- <block-item> ::= <statement> | <declaration>
parseBlockItem :: ASTParser BlockItem
parseBlockItem = State <$> parseStatement <|> Declaration <$> parseDeclaration

-- <declaration> ::= ( "int" | "string" | "bool" ) <id> [ = <exp> ] ";"
parseDeclaration :: ASTParser Declaration
parseDeclaration =
  do
    identifier <- Declare <$> parseType <*> parseIdentifierLiteral
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
                   <$> optional (parseExpression <* parseTokens [SemiColonT])
                   <*> optional (parseExpression <* parseTokens [SemiColonT])
                   <*> optional parseExpression
               )
            <* parseTokens [CloseParenthesisT]
        For expr expr' expr'' <$> parseStatement
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
      (logicalOrExpr, expr, condExpr) <-
        (,,)
          <$> (parseLogicalOrExpression <* parseTokens [QuestionMarkT])
          <*> (parseExpression <* parseTokens [ColonT])
          <*> parseConditionalExpression
      return (ConditionalExpression logicalOrExpr expr condExpr)
  )
    <|> parseLogicalOrExpression

-- <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
parseLogicalOrExpression :: ASTParser Expression
parseLogicalOrExpression =
  loop
    parseLogicalAndExpression
    ( \t e1 e2 ->
        case t of
          OrT -> Just (BinaryOperator LogicalOr e1 e2)
          _ -> Nothing
    )

-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
parseLogicalAndExpression :: ASTParser Expression
parseLogicalAndExpression =
  loop
    parseEqualityExpression
    ( \t e1 e2 ->
        case t of
          AndT -> Just (BinaryOperator LogicalAnd e1 e2)
          _ -> Nothing
    )

-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
parseEqualityExpression :: ASTParser Expression
parseEqualityExpression =
  loop
    parseRelationalExpression
    ( \t e1 e2 ->
        case t of
          NotEqualT -> Just (BinaryOperator Inequality e1 e2)
          LogicalEqualityT -> Just (BinaryOperator Equality e1 e2)
          _ -> Nothing
    )

-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
parseRelationalExpression :: ASTParser Expression
parseRelationalExpression =
  loop
    parseAdditiveExpression
    ( \t e1 e2 ->
        case t of
          LessThanT -> Just (BinaryOperator LessThan e1 e2)
          GreaterThanT -> Just (BinaryOperator GreaterThan e1 e2)
          LessThanOrEqualT -> Just (BinaryOperator LessThanOrEqual e1 e2)
          GreaterThanOrEqualT -> Just (BinaryOperator GreaterThanOrEqual e1 e2)
          _ -> Nothing
    )

-- <additive-exp> ::= <term> { ("+" | "-") <term> }
parseAdditiveExpression :: ASTParser Expression
parseAdditiveExpression =
  loop
    parseTerm
    ( \t e1 e2 ->
        case t of
          PlusT -> Just (BinaryOperator Addition e1 e2)
          MinusT -> Just (BinaryOperator Subtraction e1 e2)
          _ -> Nothing
    )

-- <term> ::= <factor> { ("*" | "/") <factor> }
parseTerm :: ASTParser Expression
parseTerm =
  loop
    parseFactor
    ( \t e1 e2 ->
        case t of
          AsteriskT -> Just (BinaryOperator Multiplication e1 e2)
          DivisionT -> Just (BinaryOperator Division e1 e2)
          _ -> Nothing
    )

-- <factor> ::= <function-call> | "(" <exp> ")" | <unary_op> <factor> | <int> | <string> | <bool> | <id>
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
    <|> (Constant <$> parseConstant)
    <|> (Variable <$> parseIdentifierLiteral)

-- <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"
parseFunctionCall :: ASTParser Expression
parseFunctionCall = do
  identifier <- parseIdentifierLiteral <* parseTokens [OpenParenthesisT]
  firstArg <- optional parseExpression
  args <- many (parseTokens [CommaT] *> parseExpression)
  parseTokens [CloseParenthesisT] $> FunCall identifier (maybeToList firstArg ++ args)

parseConstant :: ASTParser Constant
parseConstant = Parser $ \case
  (LiteralT (IntL value) : ts) -> Right (ConstantInt value, ts)
  (LiteralT (StringL value) : ts) -> Right (ConstantString value, ts)
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
