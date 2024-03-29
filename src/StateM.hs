{-# LANGUAGE TupleSections #-}

module StateM (StateM (..), get, modify) where

newtype StateM s a = StateM {runStateM :: s -> (a, s)}

instance Functor (StateM s) where
  fmap f s = StateM $ \st -> let (a, st') = runStateM s st in (f a, st')

instance Applicative (StateM s) where
  pure a = StateM (a,)
  f <*> s = StateM $ \st ->
    ( let (a, st') = runStateM s st
          (f', _) = runStateM f st
       in (f' a, st')
    )

instance Monad (StateM s) where
  return = pure
  s >>= f = StateM $ \st ->
    ( let (a, st') = runStateM s st
       in runStateM (f a) st'
    )

get :: StateM s s
get = StateM $ \s -> (s, s)

modify :: (s -> s) -> StateM s ()
modify f = StateM $ \s -> ((), f s)