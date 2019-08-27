{-# LANGUAGE LambdaCase #-}

module Ch26.EitherT where

import           Control.Monad.Trans.Class

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap fn (EitherT m) = EitherT $ (fmap . fmap) fn m

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure

  (EitherT fn) <*> (EitherT a) = EitherT $ (<*>) <$> fn <*> a

instance Monad m => Monad (EitherT e m) where
  (EitherT ema) >>= aemb = EitherT
    $ ema
    >>= (\case
           Left e  -> return $ Left e
           Right a -> runEitherT $ aemb a)

swapEither :: Either e a -> Either a e
swapEither (Left e) = Right e
swapEither (Right a) = Left a

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ema) = EitherT (swapEither <$> ema)

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT amc bmc (EitherT amb) = do
  ab <- amb
  either amc bmc ab

instance MonadTrans (EitherT a) where
  lift = EitherT . fmap Right