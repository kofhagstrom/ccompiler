module ExceptT (ExceptT (..), throwError, lift) where

newtype ExceptT e m a = ExceptT {runExceptT :: m (Either e a)}

instance (Functor m) => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

instance (Monad m) => Applicative (ExceptT e m) where
  pure = ExceptT . return . Right
  (ExceptT f) <*> (ExceptT v) =
    ExceptT $ do
      mf <- f
      either
        (return . Left)
        ( \k -> do
            mv <- v
            case mv of
              Left e -> return $ Left e
              Right x -> return $ Right (k x)
        )
        mf

instance (Monad m) => Monad (ExceptT e m) where
  return = pure
  m >>= f = ExceptT $ runExceptT m >>= either (return . Left) (runExceptT . f)

throwError :: Monad m => e -> ExceptT e m a
throwError = ExceptT . return . Left

lift :: Monad m => m a -> ExceptT e m a
lift = ExceptT . fmap Right
