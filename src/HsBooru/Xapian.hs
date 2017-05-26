{-# LANGUAGE OverloadedStrings #-}
-- | Xapian database integration
module HsBooru.Xapian
    ( XapianM
    , XapianDB
    , localDB
    , runXM
    , xapianStore
    , txBegin
    , txCommit
    ) where

import Control.Concurrent.MVar
import Control.Exception (bracket)
import Data.Char
import Data.Foldable

import System.IO.Unsafe (unsafePerformIO)
import System.FilePath.Posix (takeExtension)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Text as T

import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian.FFI

runXM :: XapianM a -> BooruM a
runXM = ioEither . runXM_

-- * Internal constants

-- Term prefix mapping, for reusable tags
booruPrefix    = "B" -- Booru name the post was scraped from
siteIDPrefix   = "I" -- The site-specific ID
uploaderPrefix = "U" -- Post uploader, ID or name (if available)
ratingPrefix   = "R" -- File rating, e.g. `safe` or `questionable`
extPrefix      = "E" -- File extension, e.g. `png`
filePrefix     = "F" -- Filename, so you can search for posts by name
tagPrefix      = ""  -- Generic content tags

-- Value mapping, for per-document unique identification and sorting
siteIDSlot   = 0 -- Site-specific ID
scoreSlot    = 1 -- User score assigned to the website
fileNameSlot = 2 -- Name the file is stored under
fileURLSlot  = 3 -- URL the file was downloaded from
sourceSlot   = 4 -- Website's literal "Source" field, if it has one

-- * Utilities

addTag :: XapianDB -> Document -> Text -> Text -> XapianM ()
addTag db doc prefix (T.map toLower -> tag) = do
    let isInvalid c | T.null prefix = not (isAlphaNum c)
                    | otherwise     = False
        validGroups = filter (not . T.null) $ T.split isInvalid tag
        safeTag     = prefix <> T.intercalate "_" validGroups
        fullTag     = prefix <> tag

    addTerm doc $ prefix <> fullTag
    unless (safeTag == fullTag) $
        addSynonym db safeTag fullTag

strVal :: Document -> ValueNumber -> Text -> XapianM ()
strVal doc val = addValStr doc val . T.map toLower

encVal :: Integral a => Document -> ValueNumber -> a -> XapianM ()
encVal doc val = addValDouble doc val . fromIntegral

-- | Encode a post and store it in the xapian database
xapianStore :: XapianDB -> Post -> XapianM ()
xapianStore _  PostDeleted{..} = return ()
xapianStore _  PostFailure{..} = return ()
xapianStore db PostSuccess{..} = do
    doc <- newDoc
    encVal doc siteIDSlot siteID
    encVal doc scoreSlot score
    strVal doc fileNameSlot fileName
    strVal doc fileURLSlot  fileURL
    forM_ source $ strVal doc sourceSlot

    addTag db doc booruPrefix    $ T.pack postSite
    addTag db doc siteIDPrefix   $ T.pack (show siteID)
    addTag db doc uploaderPrefix $ T.pack (show uploader)
    addTag db doc ratingPrefix   $ T.pack (show rating)
    addTag db doc filePrefix     $ fileName
    addTag db doc extPrefix      $ getExt fileName
    forM_ tags $ addTag db doc tagPrefix

    void $ addDocument db doc

getExt :: Text -> Text
getExt = T.pack . drop 1 . takeExtension . T.unpack