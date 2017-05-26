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
    , forConcurrentlyB
    ) where

import Prelude hiding (log)

import Control.Exception
import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Data.Time (getZonedTime)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import HsBooru.Types

-- | Requires the value to be Just, or throws the given String on failure
--
-- >>> runBooruM . requireJust "err" $ return (Just 3)
-- Right 3
--
-- >>> runBooru M . requireJust "err" $ return Nothing
-- Left "err"
requireJust :: String -> IO (Maybe a) -> BooruM a
requireJust err = maybe (throwB err) pure <=< io

-- | Requires the value to be Right, or throws the Left result on failure
--
-- >>> runBooruM . requireRight $ Identity (Right 4)
-- Right 4
--
-- >>> runBooruM . requireRight $ Identity (Left False)
-- Left "False"
requireRight :: Show e => IO (Either e a) -> BooruM a
requireRight = either (throwB . show) pure <=< io

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

-- | Catches all IOExceptions and turns them into pure exceptions
ioCatch :: IO a -> BooruM a
ioCatch act = ioEither $ try act <&> lmap (show :: IOException -> String)

lmap :: (a -> b) -> Either a x -> Either b x
lmap f (Left  x) = Left (f x)
lmap _ (Right x) = Right x

-- | When used with ExceptT, this retries an action multiple times
retry :: Int -> BooruM a -> BooruM a
retry 1 a = a
retry n a = a `catchB` (\_ -> retry (n-1) a)

-- | Like forConcurrently, but works for BooruM instead of IO. If
-- any thread throws an exception, it will be rethrown once all threads
-- complete.
forConcurrentlyB :: Traversable t => t a -> (a -> BooruM b) -> BooruM (t b)
forConcurrentlyB ts f = recombine distribute
    where distribute = mapConcurrently (runBooruM . f) ts
          recombine  = ioEither . fmap sequence
