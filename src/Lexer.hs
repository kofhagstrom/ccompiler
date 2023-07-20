module Lexer
  ( lexChars,
    Lexer,
    Keyword (..),
    Token (..),
    Literal (..),
  )
where

import Data.Char (isAlphaNum, isDigit)
import Data.Map (Map, fromList, lookup)

data Token
  = SpaceT
  | OpenBraceT
  | CloseBraceT
  | OpenParenthesisT
  | CloseParenthesisT
  | SemiColonT
  | PlusT
  | AsteriskT
  | DivisionT
  | MinusT
  | BangT
  | TildeT
  | LogicalEqualityT
  | InequalityT
  | LessThanT
  | GreaterThanT
  | GreaterThanOrEqualT
  | LessThanOrEqualT
  | NotEqualT
  | AndT
  | OrT
  | AssignmentT
  | ColonT
  | QuestionMarkT
  | KeywordT Keyword
  | LiteralT Literal
  | ErrorT String
  deriving (Show, Eq)

data Keyword
  = IntKW
  | ReturnKW
  | IfKW
  | ElseKW
  deriving (Show, Eq)

data Literal
  = IntL Integer
  | StringL String
  | IdentifierL String
  deriving (Show, Eq)

type LexRow = String -> [Token]

keywords :: Map String Keyword
keywords =
  fromList
    [ ("return", ReturnKW),
      ("int", IntKW),
      ("if", IfKW),
      ("else", ElseKW)
    ]

singleCharTokens :: Map Char Token
singleCharTokens =
  fromList
    [ (' ', SpaceT),
      ('(', OpenParenthesisT),
      (')', CloseParenthesisT),
      ('{', OpenBraceT),
      ('}', CloseBraceT),
      (';', SemiColonT),
      ('-', MinusT),
      ('!', BangT),
      ('~', TildeT),
      ('+', PlusT),
      ('*', AsteriskT),
      ('/', DivisionT),
      ('?', QuestionMarkT),
      (':', ColonT)
    ]

loxMultiCharTokens :: Map Char LexRow
loxMultiCharTokens =
  fromList
    [ ('!', lexBang),
      ('/', lexSlash),
      ('<', lexLessThan),
      ('>', lexGreaterThan),
      ('&', lexAnd),
      ('|', lexOr),
      ('=', lexLogicalEquality)
    ]

type Lexer = [String] -> Either String [Token]

lexChars :: Lexer
lexChars strings = sequence . checkForErrors $ map lexRow strings

lexRow :: LexRow
lexRow string =
  if null string
    then []
    else case token of
      Just tok ->
        if tok == SpaceT
          then lexRow rest
          else tok : lexRow rest
      Nothing -> lexMultiCharToken string
  where
    (char : rest) = string
    token = Data.Map.lookup char singleCharTokens

lexMultiCharToken :: LexRow
lexMultiCharToken string =
  case maybeFun of
    Just fun -> fun rest
    Nothing -> lexRest
  where
    (char : rest) = string
    maybeFun = Data.Map.lookup char loxMultiCharTokens
    lexRest
      | isDigit char = lexIntegerLiteral string
      | isAlphaNum char = lexIdentifiersAndKeywords string
      | otherwise = [ErrorT $ "Invalid token: " ++ [char]]

lexAnd :: LexRow
lexAnd [] = undefined
lexAnd (x : xs)
  | x == '&' = AndT : lexRow xs
  | otherwise = [ErrorT $ "Invalid token: " ++ [x]]

lexLogicalEquality :: LexRow
lexLogicalEquality [] = undefined
lexLogicalEquality chars@(x : xs)
  | x == '=' = LogicalEqualityT : lexRow xs
  | otherwise = AssignmentT : lexRow chars

lexOr :: LexRow
lexOr [] = undefined
lexOr (x : xs)
  | x == '|' = OrT : lexRow xs
  | otherwise = [ErrorT $ "Invalid token " ++ [x]]

lexBang :: LexRow
lexBang [] = undefined
lexBang (x : xs)
  | x == '=' = NotEqualT : lexRow xs
  | otherwise = BangT : lexRow xs

lexSlash :: LexRow
lexSlash [] = undefined
lexSlash string@(x : _)
  | x == '/' = []
  | otherwise = DivisionT : lexRow string

lexGreaterThan :: LexRow
lexGreaterThan [] = undefined
lexGreaterThan string@(x : xs)
  | x == '=' = GreaterThanOrEqualT : lexRow xs
  | otherwise = GreaterThanT : lexRow string

lexLessThan :: LexRow
lexLessThan [] = undefined
lexLessThan string@(x : xs)
  | x == '=' = LessThanOrEqualT : lexRow xs
  | otherwise = LessThanT : lexRow string

lexIntegerLiteral :: LexRow
lexIntegerLiteral string = (LiteralT . IntL . read) prefix : lexRow suffix
  where
    (prefix, suffix) = span isAlphaNum string

lexIdentifiersAndKeywords :: LexRow
lexIdentifiersAndKeywords string =
  case keyword of
    Just kw -> KeywordT kw : lexRow suffix
    Nothing -> LiteralT (IdentifierL prefix) : lexRow suffix
  where
    keyword = Data.Map.lookup prefix keywords
    (prefix, suffix) = span isAlphaNumOrUnderscore string
    isAlphaNumOrUnderscore c = isAlphaNum c || c == '_'

type ErrorMessage = String

checkForErrors :: [[Token]] -> [Either ErrorMessage Token]
checkForErrors [] = []
checkForErrors tokens = concatMap checkRow (zip [1 ..] tokens)

checkRow :: (Integer, [Token]) -> [Either ErrorMessage Token]
checkRow (_, []) = []
checkRow (n, t : ts) =
  let rest = (n, ts)
   in case t of
        ErrorT err -> Left ("Error on line " <> show n <> ": " ++ err) : checkRow rest
        _ -> Right t : checkRow rest