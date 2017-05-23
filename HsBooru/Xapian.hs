-- | Xapian database integration
module HsBooru.Xapian
    ( XapianDB
    , xapianDB
    , runXM
    , xapianStore
    , commit
    ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.Char
import Data.Foldable

import System.IO.Unsafe (unsafePerformIO)
import System.FilePath.Posix (takeExtension)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as T

import HsBooru.Conf
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian.FFI

-- Utilities

addTag :: Document -> Text -> Text -> XapianM ()
addTag doc prefix = addTerm doc . (prefix <>) . T.map fix
    where fix ' ' = '_'
          fix c   = toLower c

strVal :: Document -> ValueNumber -> Text -> XapianM ()
strVal doc val = addValStr doc val . T.map toLower

encVal :: Integral a => Document -> ValueNumber -> a -> XapianM ()
encVal doc val = addValDouble doc val . fromIntegral

-- | Encode a post and store it in the xapian database
xapianStore :: XapianDB -> Post -> XapianM ()
xapianStore db Post{..} = do
    doc <- newDoc
    encVal doc siteIDSlot siteID
    encVal doc scoreSlot score
    strVal doc fileNameSlot fileName
    strVal doc fileURLSlot  fileURL
    forM_ source $ strVal doc sourceSlot

    addTag doc booruPrefix    $ T.pack booru
    addTag doc siteIDPrefix   $ T.pack (show siteID)
    addTag doc uploaderPrefix $ T.pack (show uploader)
    addTag doc ratingPrefix   $ T.pack (show rating)
    addTag doc filePrefix     $ fileName
    addTag doc extPrefix      $ getExt fileName
    forM_ tags $ addTag doc tagPrefix

    addDocument db doc

getExt :: Text -> Text
getExt = T.pack . drop 1 . takeExtension . T.unpack
