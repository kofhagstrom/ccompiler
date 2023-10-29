{-# LANGUAGE TupleSections #-}

module State (State (..), get, modify) where

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f s = State $ \st -> let (a, st') = runState s st in (f a, st')

instance Applicative (State s) where
  pure a = State (a,)
  f <*> s = State $ \st ->
    ( let (a, st') = runState s st
          (f', _) = runState f st
       in (f' a, st')
    )

instance Monad (State s) where
  return = pure
  s >>= f = State $ \st ->
    ( let (a, st') = runState s st
       in runState (f a) st'
    )

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)