module Ch26.Excersise where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  a <- ask
  lift $ putStrLn $ "Hi: " ++ show a
  return (a + 1)

