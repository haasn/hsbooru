-- | Miscellaneous helpers
module HsBooru.Util
    ( requireJust
    , requireRight
    , log
    , logError
    , (<&>)
    , io
    , ioCatch
    , retry
    , forConcurrentlyE
    ) where

import Prelude hiding (log)

import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class

import Data.Time (getZonedTime)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import HsBooru.Types

-- | Requires the value to be Just, or throws the given String on failure
requireJust :: String -> IO (Maybe a) -> ExceptT String IO a
requireJust err = maybe (throwE err) pure <=< io

-- | Requires the value to be Right, or throws the Left result on failure
requireRight :: Show e => IO (Either e a) -> ExceptT String IO a
requireRight = either (throwE . show) pure <=< io

log, logError :: String -> String -> IO ()
log      name msg = hPutStrLn' stdout $ "["++name++"] " ++ msg
logError name err = hPutStrLn' stderr $ "["++name++"] Error: " ++ err

hPutStrLn' :: Handle -> String -> IO ()
hPutStrLn' h str = withLock $ do
    t <- getZonedTime
    hPutStrLn h $ show t ++ " " ++ str
    where withLock = bracket (takeMVar ioLock) (putMVar ioLock) . const

ioLock :: MVar ()
ioLock = unsafePerformIO $ newMVar ()

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Catches all IOExceptions and turns them into ExceptT Strings
ioCatch :: IO a -> ExceptT String IO a
ioCatch act = ExceptT $ try act <&> lmap (show :: IOException -> String)

lmap :: (a -> b) -> Either a x -> Either b x
lmap f (Left  x) = Left (f x)
lmap _ (Right x) = Right x

-- | When used with ExceptT, this retries an action multiple times
retry :: MonadPlus m => Int -> m a -> m a
retry n = msum . replicate n

-- | Like forConcurrently, but works for ExceptT e IO instead of just IO. If
-- any thread throws an exception, it will be rethrown once all threads
-- complete.
forConcurrentlyE :: Traversable t => t a -> (a -> ExceptT e IO b) -> ExceptT e IO (t b)
forConcurrentlyE ts f = recombine distribute
    where distribute = mapConcurrently (runExceptT . f) ts
          recombine  = ExceptT . fmap sequence
