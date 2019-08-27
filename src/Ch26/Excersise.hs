module Ch26.Excersise where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Maybe
import           Control.Monad
import           Ch26.StateT
import           Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = reader (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = reader show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ask
  >>= (\a -> (lift $ putStrLn $ "Hi: " ++ show a) >> (return (a + 1)))

state :: (Monad m) => (s -> (a, s)) -> StateT s m a
state f = StateT $ \s -> return (f s)

get :: (Monad m) => StateT s m s
get = state (\s -> (s, s))

put :: (Monad m) => s -> (StateT s m ())
put s = state $ const ((), s)

sPrintInAccum :: (Num a, Show a) => StateT a IO String
sPrintInAccum = get
  >>= (\a -> (lift $ putStrLn $ "Hi: " ++ show a)
       >> put (a + 1)
       >> return (show a))

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)