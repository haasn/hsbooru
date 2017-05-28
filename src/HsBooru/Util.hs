-- | Miscellaneous helpers
module HsBooru.Util
    ( log
    , logError
    , (<&>)
    , io
    , ioCatch
    , withReturn
    ) where

import Prelude hiding (log)

import Control.Exception
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont

import Data.Time (getZonedTime)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

import HsBooru.Types

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
ioCatch :: HasCallStack => IO a -> BooruM a
ioCatch act = ioEither $ try act <&> lmap (show :: IOException -> String)

lmap :: (a -> b) -> Either a x -> Either b x
lmap f (Left  x) = Left (f x)
lmap _ (Right x) = Right x

withReturn :: Monad m => ((r -> ContT r m b) -> ContT r m r) -> m r
withReturn = evalContT . callCC
