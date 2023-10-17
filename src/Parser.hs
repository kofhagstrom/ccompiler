module Parser (Parser (..)) where

import Control.Applicative

newtype Parser a e x = Parser {runParser :: [a] -> Either ([e], [a]) (x, [a])}

instance Functor (Parser a e) where
  fmap f p = Parser $ \input -> do
    (x, input') <- runParser p input
    return (f x, input')

instance Applicative (Parser a e) where
  pure a = Parser $ \input -> Right (a, input)
  p1 <*> p2 = Parser $ \input -> do
    (f, input') <- runParser p1 input
    (a, input'') <- runParser p2 input'
    return (f a, input'')

instance Alternative (Parser a e) where
  empty = Parser $ \_ -> Left ([], [])
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Right a -> Right a
      Left (e, _) -> case runParser p2 input of
        Right a' -> Right a'
        Left (e', ts') -> Left (e <> e', ts')

instance Monad (Parser a e) where
  return = pure
  p >>= f = Parser $ \input -> do
    (a, input') <- runParser p input
    runParser (f a) input'