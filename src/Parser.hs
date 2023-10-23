module Parser (Parser (..)) where

import Control.Applicative

newtype Parser i e o = Parser {run :: i -> Either (e, i) (o, i)}

instance (Monoid i, Monoid e) => Functor (Parser i e) where
  fmap f p = Parser $ \input -> do
    (x, input') <- run p input
    return (f x, input')

instance (Monoid i, Monoid e) => Applicative (Parser i e) where
  pure a = Parser $ \input -> Right (a, input)
  p1 <*> p2 = Parser $ \input -> do
    (f, input') <- run p1 input
    (a, input'') <- run p2 input'
    return (f a, input'')

instance (Monoid i, Monoid e) => Alternative (Parser i e) where
  empty = Parser $ \_ -> Left (mempty, mempty)
  p1 <|> p2 = Parser $ \input ->
    case run p1 input of
      Right a -> Right a
      Left (e, _) -> case run p2 input of
        Right a' -> Right a'
        Left (e', ts') -> Left (e <> e', ts')

instance (Monoid i, Monoid e) => Monad (Parser i e) where
  return = pure
  p >>= f = Parser $ \input -> do
    (a, input') <- run p input
    run (f a) input'