{-# LANGUAGE LambdaCase #-}

module Parser
  ( Parser (..),
    program,
    Program (..),
    FuncDeclaration (..),
    Statement (..),
    Expression (..),
    UnaryOperator (..),
    BinaryOperator (..),
    BlockItem (..),
    Declaration (..),
    TopLevelItem (..),
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
import Lexer
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
import Parsec (ParseError (..), Parser (..), loop, oneOf, parseC)

type ASTParser a = Parser [Token] a

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
  = ConstantInt String
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
program :: ASTParser Program
program = Program <$> many (F <$> funcDeclaration <|> D <$> declaration)

ctype :: ASTParser CType
ctype = do
  t <- oneOf [KeywordT IntKW, KeywordT StringKW]
  case t of
    [KeywordT IntKW] -> return CInt
    [KeywordT StringKW] -> return CString
    _ -> empty

-- <function> ::= ( "int" | "string" | "bool" ) <id> "(" [ "int" <id> { "," "int" <id> } ] ")" ( "{" { <block-item> } "}" | ";" )
funcDeclaration :: ASTParser FuncDeclaration
funcDeclaration = do
  returnType <- ctype
  identifier <- identifierLiteral
  firstArg <- tokens [OpenParenthesisT] *> optional (tokens [KeywordT IntKW] *> identifierLiteral)
  args <- many (tokens [CommaT, KeywordT IntKW] *> identifierLiteral) <* tokens [CloseParenthesisT, OpenBraceT]
  Fun returnType identifier (maybeToList firstArg ++ args) <$> (optional (many blockItem) <* tokens [CloseBraceT])

-- <block-item> ::= <statement> | <declaration>
blockItem :: ASTParser BlockItem
blockItem = State <$> statement <|> Declaration <$> declaration

-- <declaration> ::= ( "int" | "string" | "bool" ) <id> [ = <exp> ] ";"
declaration :: ASTParser Declaration
declaration =
  do
    identifier <- Declare <$> ctype <*> identifierLiteral
    expr <- optional (tokens [AssignmentT] *> expression) <* tokens [SemiColonT]
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
statement :: ASTParser Statement
statement =
  return_
    <|> while
    <|> do_
    <|> break_
    <|> continue
    <|> compoune
    <|> for
    <|> forDeclaration
    <|> optionalExpression
    <|> conditional
  where
    -- "if" "(" <exp> ")" <statement> [ "else" <statement> ]
    conditional =
      Conditional
        <$> ( tokens [KeywordT IfKW, OpenParenthesisT]
                *> expression
                <* tokens [CloseParenthesisT]
            )
        <*> statement
        <*> optional (tokens [KeywordT ElseKW] *> statement)
    -- <exp-option> ";"
    optionalExpression = Expression <$> (optional expression <* tokens [SemiColonT])
    -- "return" <exp> ";"
    return_ =
      Return
        <$> ( tokens [KeywordT ReturnKW]
                *> expression
                <* tokens [SemiColonT]
            )
    -- "while" "(" <exp> ")" <statement>
    while =
      While
        <$> (tokens [KeywordT WhileKW, OpenParenthesisT] *> expression <* tokens [CloseParenthesisT])
        <*> statement
    -- "do" <statement> "while" "(" <exp> ")" ";"
    do_ =
      Do
        <$> (tokens [KeywordT DoKW] *> statement <* tokens [KeywordT WhileKW, OpenParenthesisT])
        <*> (expression <* tokens [CloseParenthesisT, SemiColonT])
    -- "{" { <block-item> } "}"
    compoune =
      Compound
        <$> ( tokens [OpenBraceT]
                *> many blockItem
                <* tokens [CloseBraceT]
            )
    -- "for" "(" <exp-option> ";" <exp-option> ";" <exp-option> ")" <statement>
    for =
      do
        (expr, expr', expr'') <-
          tokens [KeywordT ForKW, OpenParenthesisT]
            *> ( (,,)
                   <$> optional (expression <* tokens [SemiColonT])
                   <*> optional (expression <* tokens [SemiColonT])
                   <*> optional expression
               )
            <* tokens [CloseParenthesisT]
        For expr expr' expr'' <$> statement
    -- "for" "(" <declaration> <exp-option> ";" <exp-option> ")" <statement>
    forDeclaration =
      do
        decl <- tokens [KeywordT ForKW, OpenParenthesisT] *> declaration
        (expr, expr') <-
          (,)
            <$> optional (expression <* tokens [SemiColonT])
            <*> (optional expression <* tokens [CloseParenthesisT])
        ForDecl decl expr expr' <$> statement
    -- "break" ";"
    break_ = tokens [KeywordT BreakKW, SemiColonT] $> Break
    -- "continue" ";"
    continue = tokens [KeywordT ContinueKW, SemiColonT] $> Continue

-- <exp> ::= <id> "=" <exp> | <conditional-exp>
expression :: ASTParser Expression
expression =
  Assign <$> (identifierLiteral <* tokens [AssignmentT]) <*> expression
    <|> conditionalExpression

-- <conditional-exp> ::= <logical-or-exp> [ "?" <exp> ":" <conditional-exp> ]
conditionalExpression :: ASTParser Expression
conditionalExpression =
  ( do
      (logicalOrExpr, expr, condExpr) <-
        (,,)
          <$> (logicalOrExpression <* tokens [QuestionMarkT])
          <*> (expression <* tokens [ColonT])
          <*> conditionalExpression
      return (ConditionalExpression logicalOrExpr expr condExpr)
  )
    <|> logicalOrExpression

-- <logical-or-exp> ::= <logical-and-exp> { "||" <logical-and-exp> }
logicalOrExpression :: ASTParser Expression
logicalOrExpression =
  loop
    logicalAndExpression
    ( \t e1 e2 ->
        case t of
          OrT -> Just (BinaryOperator LogicalOr e1 e2)
          _ -> Nothing
    )

-- <logical-and-exp> ::= <equality-exp> { "&&" <equality-exp> }
logicalAndExpression :: ASTParser Expression
logicalAndExpression =
  loop
    equalityExpression
    ( \t e1 e2 ->
        case t of
          AndT -> Just (BinaryOperator LogicalAnd e1 e2)
          _ -> Nothing
    )

-- <equality-exp> ::= <relational-exp> { ("!=" | "==") <relational-exp> }
equalityExpression :: ASTParser Expression
equalityExpression =
  loop
    relationalExpression
    ( \t e1 e2 ->
        case t of
          NotEqualT -> Just (BinaryOperator Inequality e1 e2)
          LogicalEqualityT -> Just (BinaryOperator Equality e1 e2)
          _ -> Nothing
    )

-- <relational-exp> ::= <additive-exp> { ("<" | ">" | "<=" | ">=") <additive-exp> }
relationalExpression :: ASTParser Expression
relationalExpression =
  loop
    additiveExpression
    ( \t e1 e2 ->
        case t of
          LessThanT -> Just (BinaryOperator LessThan e1 e2)
          GreaterThanT -> Just (BinaryOperator GreaterThan e1 e2)
          LessThanOrEqualT -> Just (BinaryOperator LessThanOrEqual e1 e2)
          GreaterThanOrEqualT -> Just (BinaryOperator GreaterThanOrEqual e1 e2)
          _ -> Nothing
    )

-- <additive-exp> ::= <term> { ("+" | "-") <term> }
additiveExpression :: ASTParser Expression
additiveExpression =
  loop
    term
    ( \t e1 e2 ->
        case t of
          PlusT -> Just (BinaryOperator Addition e1 e2)
          MinusT -> Just (BinaryOperator Subtraction e1 e2)
          _ -> Nothing
    )

-- <term> ::= <factor> { ("*" | "/") <factor> }
term :: ASTParser Expression
term =
  loop
    factor
    ( \t e1 e2 ->
        case t of
          AsteriskT -> Just (BinaryOperator Multiplication e1 e2)
          DivisionT -> Just (BinaryOperator Division e1 e2)
          _ -> Nothing
    )

-- <factor> ::= <function-call> | "(" <exp> ")" | <unary_op> <factor> | <int> | <string> | <bool> | <id>
factor :: ASTParser Expression
factor =
  functionCall
    <|> tokens [OpenParenthesisT] *> expression <* tokens [CloseParenthesisT]
    <|> ( oneOf [BangT, MinusT, TildeT]
            >>= \case
              [BangT] -> UnaryOperator LogicalNegation <$> factor
              [MinusT] -> UnaryOperator Negation <$> factor
              [TildeT] -> UnaryOperator BitwiseComplement <$> factor
              _ -> empty
        )
    <|> (Constant <$> constant)
    <|> (Variable <$> identifierLiteral)

-- <function-call> ::= id "(" [ <exp> { "," <exp> } ] ")"
functionCall :: ASTParser Expression
functionCall = do
  identifier <- identifierLiteral <* tokens [OpenParenthesisT]
  firstArg <- optional expression
  args <- many (tokens [CommaT] *> expression)
  tokens [CloseParenthesisT] $> FunCall identifier (maybeToList firstArg ++ args)

constant :: ASTParser Constant
constant = Parser $ \case
  (LiteralT (IntL value) : ts) -> Right (ConstantInt value, ts)
  (LiteralT (StringL value) : ts) -> Right (ConstantString value, ts)
  ts@(t : _) -> Left ([UnexpectedError ("Expected LiteralT (IntL _), got " ++ show t)], ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

identifierLiteral :: ASTParser String
identifierLiteral = Parser $ \case
  (LiteralT (IdentifierL identifier) : ts) -> Right (identifier, ts)
  ts@(t : _) -> Left ([UnexpectedError ("Expected LiteralT (IdentifierL _), got " ++ show t)], ts)
  [] -> Left ([UnexpectedError "Expected something, got nothing."], [])

tokens :: [Token] -> ASTParser [Token]
tokens = traverse token

token :: Token -> ASTParser Token
token t = parseC (t ==)
