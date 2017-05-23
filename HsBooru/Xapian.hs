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

import HsBooru.Conf
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian.FFI

-- Utilities

addTag :: Document -> String -> String -> XapianM ()
addTag doc prefix = addTerm doc . (prefix ++) . map fix
    where fix ' ' = '_'
          fix c   = toLower c

strVal :: Document -> ValueNumber -> String -> XapianM ()
strVal doc val = addValStr doc val . map toLower

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

    addTag doc booruPrefix    $ booru
    addTag doc siteIDPrefix   $ show siteID
    addTag doc uploaderPrefix $ show uploader
    addTag doc ratingPrefix   $ show rating
    addTag doc filePrefix     $ fileName
    addTag doc extPrefix      $ drop 1 (takeExtension fileName)
    forM_ tags $ addTag doc tagPrefix

    addDocument db doc
