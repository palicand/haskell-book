{-# LANGUAGE OverloadedStrings #-}
module Ch26.HitCounter where
import           Control.Monad.Trans.Class
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map.Strict               as M
import           Data.Text.Lazy                 ( Text )
import qualified Data.Text.Lazy                as TL
import           Web.Scotty.Trans

data Config = Config { counts :: IORef (M.Map Text Integer) , prefix :: Text }


type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)
bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k newVal m, newVal)
    where newVal = M.findWithDefault 0 k m + 1

app :: Scotty ()
app = get "/:key" $ do
    conf       <- lift ask
    unprefixed <- param "key"
    counts'    <- liftIO $ readIORef (counts conf)
    let key'                 = mappend (prefix conf) unprefixed
        (newMap, newInteger) = bumpBoomp key' counts'
    liftIO $ writeIORef (counts conf) newMap
    html $ mconcat
        [ "<h1>Success! Count for = "
        , key'
        , " was = "
        , TL.pack $ show newInteger
        , "</h1>"
        ]

main :: String -> IO ()
main prefixArg = do
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR response = runReaderT response config
    scottyT 3000 runR app
