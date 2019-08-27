module Ch26.ReaderT where

import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure

  (ReaderT f) <*> (ReaderT rma) = ReaderT $ (<*>) <$> f <*> rma

instance (Monad m) => Monad (ReaderT r m) where
  (ReaderT rma) >>= f = ReaderT $ \r -> rma r >>= (\a -> (runReaderT (f a) r))

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = ReaderT . return . liftIO