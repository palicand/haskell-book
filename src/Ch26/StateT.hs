{-# LANGUAGE TupleSections #-}

module Ch26.StateT where

import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ (fmap . fmap) (\(a, s) -> (f a, s)) smas

instance (Monad m) => Applicative (StateT s m) where
  pure a = StateT $ \s -> pure (a, s)

  (StateT f) <*> (StateT smas) = StateT
    $ \s -> do
      (ab, s') <- f s
      (a, s'') <- smas s'
      return (ab a, s'')

instance (Monad m) => Monad (StateT s m) where
  (StateT smas) >>= asmb = StateT
    $ \s -> smas s >>= (\(a, s') -> (runStateT (asmb a) s'))

           -- instance MonadTrans (StateT s) where
           --   lift ma = StateT $ \s -> fmap (, s) ma
instance MonadTrans (StateT s) where
  lift = StateT . (\ma s -> fmap (, s) ma)

instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO