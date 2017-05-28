{-# LANGUAGE OverloadedStrings #-}
-- | Xapian database integration
module HsBooru.Xapian
    ( XapianM
    , XapianDB
    , localDB
    , memoryDB
    , runXM
    , xapianStore
    , txBegin
    , txCommit
    , sanitizeTag
    ) where

import Data.Char
import Data.Foldable
import Data.Time.Clock.POSIX
import System.FilePath.Posix (takeExtension)
import qualified Data.Text as T

import HsBooru.Types
import HsBooru.Xapian.FFI

runXM :: XapianM a -> BooruM a
runXM = ioEither . runXM_

-- * Internal constants

-- Term prefix mapping, for reusable tags
booruPrefix    = "B" -- Booru name the post was scraped from
siteIDPrefix   = "I" -- The site-specific ID
uploaderPrefix = "U" -- Post uploader, in the format id@site
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
uploadedSlot = 5 -- Upload timestamp

-- * Utilities

addTag :: XapianDB -> Document -> Text -> Text -> XapianM ()
addTag db doc prefix (T.map toLower -> tag) = do
    let fullTag = prefix <> tag
        safeTag | T.null prefix = sanitizeTag tag
                | otherwise     = fullTag

    addTerm doc $ fullTag
    unless (safeTag == fullTag) $
        addSynonym db safeTag fullTag

-- | Sanitize a tag by reducing it to the groups of of valid characters,
-- separated by _ characters. If the tag has no valid characters, the result is
-- simply "_" itself.
sanitizeTag :: Text -> Text
sanitizeTag tag | null validGroups = "_"
                | otherwise        = T.intercalate "_" validGroups
    where validGroups = filter (not . T.null) $ T.split (not . isAlphaNum) tag

strVal :: Document -> ValueNumber -> Text -> XapianM ()
strVal doc val = addValStr doc val . T.map toLower

encVal :: Real a => Document -> ValueNumber -> a -> XapianM ()
encVal doc val = addValDouble doc val . realToFrac

-- | Encode a post and store it in the xapian database
xapianStore :: XapianDB -> Post -> XapianM ()
xapianStore _  PostDeleted{..} = return ()
xapianStore _  PostFailure{..} = return ()
xapianStore db PostSuccess{..} = do
    doc <- newDoc
    encVal doc siteIDSlot siteID
    encVal doc scoreSlot score
    encVal doc uploadedSlot $ utcTimeToPOSIXSeconds uploaded
    strVal doc fileNameSlot fileName
    strVal doc fileURLSlot  fileURL
    forM_ source $ strVal doc sourceSlot

    addTag db doc booruPrefix    $ T.pack postSite
    addTag db doc siteIDPrefix   $ T.pack (show siteID)
    addTag db doc uploaderPrefix $ T.pack (show uploader ++ "@" ++ postSite)
    addTag db doc ratingPrefix   $ T.pack (show rating)
    addTag db doc filePrefix     $ fileName
    addTag db doc extPrefix      $ getExt fileName
    forM_ tags $ addTag db doc tagPrefix

    void $ addDocument db doc

getExt :: Text -> Text
getExt = T.pack . drop 1 . takeExtension . T.unpack
