-- | Xapian database integration
module HsBooru.Xapian
    ( xapianDB
    , runXM
    , xapianStore
    , commit
    ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.Char
import Data.Foldable

import Search.Xapian
import System.IO.Unsafe (unsafePerformIO)
import System.FilePath.Posix (takeExtension)
import qualified Data.ByteString.UTF8 as UTF8

import HsBooru.Conf
import HsBooru.Types
import HsBooru.Util

-- Utilities

addTag :: Document -> String -> String -> XapianM ()
addTag doc prefix str = addTerm (UTF8.fromString sanitized) doc
    where sanitized = prefix ++ map fix str

          fix ' ' = '_'
          fix c   = toLower c

strVal :: Document -> ValueNumber -> String -> XapianM ()
strVal doc val str = setValue val (UTF8.fromString sanitized) doc
    where sanitized = map toLower str

encVal :: Integral a => Document -> ValueNumber -> a -> XapianM ()
encVal doc val num = do
    enc <- io $ sortableSerialise (fromIntegral num)
    setValue val enc doc

-- | Encode a post and store it in the xapian database
xapianStore :: XapianDB -> Post -> XapianM ()
xapianStore db Post{..} = do
    doc <- emptyDocument
    encVal doc siteIDSlot siteID
    encVal doc scoreSlot score
    strVal doc fileNameSlot fileName
    strVal doc fileURLSlot  fileURL
    forM_ source $ strVal doc sourceSlot

    addTag doc booruPrefix    $ booru
    addTag doc siteIDPrefix   $ show siteID
    addTag doc uploaderPrefix $ show uploader
    addTag doc ratingPrefix   $ show rating
    addTag doc filePrefix     $ fileName
    addTag doc extPrefix      $ drop 1 (takeExtension fileName)
    forM_ tags $ addTag doc tagPrefix

    void $ addDocument db doc

-- | Open a new xapian database instance. This is GC'd and can not be held
-- concurrently
xapianDB :: ExceptT String IO XapianDB
xapianDB = requireRight $ openReadWrite CreateOrOpen xapianDir

-- Xapian is not thread safe at all, so we just take a global lock on runXM
-- to prevent it from blowing up
runXM :: XapianM a -> ExceptT String IO a
runXM = ioCatch . withLock . runXapian
    where withLock = bracket (takeMVar xapianLock) (putMVar xapianLock) . const

xapianLock :: MVar ()
xapianLock = unsafePerformIO $ newMVar ()
