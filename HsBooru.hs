{-# LANGUAGE TemplateHaskell, RecordWildCards, PatternSynonyms, ViewPatterns #-}

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Data.Time
import Data.SafeCopy
import Data.Char (isNumber)
import Data.List (find, isPrefixOf)
import Data.Maybe (listToMaybe)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Network.Curl ( CurlCode(..), CurlOption(..)
                    , CurlResponse_(..), curlGetResponse_ )

import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), takeFileName)

import Text.Read (readMaybe)
import Text.HTML.Scalpel hiding (curlOpts)

-- ^ Global configuration

-- Directories
dbDir     = "/z/booru/"
imageDir  = dbDir </> "images"
xapianDir = dbDir </> "xapian"

curlOpts  = [CurlFollowLocation True]

-- ^ Types and instances

-- This is defined via SafeCopy so I can migrate to a newer version
-- in the future

data FileInfo = FileInfo { fileName :: !T.Text } deriving Show

deriveSafeCopy 0 'base ''FileInfo

-- These types are only used within the scraper, and don't need to be stored

data Post = Post
    { rating   :: !Rating
    , uploader :: !String
    , score    :: !Int
    , source   :: !(Maybe String)
    , fileURL  :: !String
    , tags     :: ![Tag]
    } deriving Show

data Rating = Safe | Questionable | Explicit
    deriving (Show, Read)

data Tag = Tag
    { tagType :: !TagType
    , tagName :: !String
    } deriving Show

data TagType = General | Character | Artist | Work
    deriving (Show, Read)

data SiteScraper = SiteScraper
    { siteName    :: String
    , scrapeID    :: Int -> IO (Maybe Post)
    , scrapeCount :: IO (Maybe Int)
    }

-- ^ Scraper definitions

-- Helper functions
extractID :: URL -> Maybe Int
extractID = readMaybe . dropWhile (not . isNumber)

getRating :: String -> Maybe Rating
getRating = chomp "Rating: " >=> readMaybe

chomp :: String -> String -> Maybe String
chomp [] ys = Just ys
chomp _  [] = Nothing
chomp (x:xs) (y:ys)
    | x == y    = chomp xs ys
    | otherwise = Nothing

-- More useful scraper primitives
textMaybe :: Selectable s => s -> Scraper String (Maybe String)
textMaybe s = Just <$> text s <|> pure Nothing

textFirst :: Selectable s => (String -> Maybe a) -> s -> Scraper String a
textFirst f s = texts s >>= go
    where go [] = mzero
          go ((f -> Just x):xs) = pure x
          go (_:xs) = go xs

textRead :: (Selectable s, Read a) => s -> Scraper String a
textRead = maybe mzero pure . readMaybe <=< text

(@^=) :: String -> String -> AttributePredicate
k @^= v = match $ \k' v' -> k == k' && v `isPrefixOf` v'

-- Gelbooru scraper
gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"

          scrapeCount = scrapeURL rssUrl $ do
                newestID <- fmap extractID . text $ "item" // "link"
                case newestID of
                    Just id -> return id
                    Nothing -> mzero

          -- NOTE: gelbooru has a programmable API which would make
          -- most of this easier, but unfortunately it has some limitations
          -- so we're stuck with manually parsing the website if we want
          -- all of the information..
          scrapeID postID = scrapeURL (postUrl ++ show postID) $ do
                -- Extract the stats by parsing the list on the left
                let stats = "div" @: ["id" @= "stats"] // "li"
                uploader <- text $ stats // "a" -- luckily the first link
                rating   <- textFirst getRating stats
                score    <- textRead  $ stats // "span" @: ["id" @^= "psc"]
                source   <- textMaybe $ stats // "a" @: ["rel" @= "nofllow"]

                -- This is the easiest way to match the "Original image" link
                fileURL <- attr "href" $ "div" // "li" // "a"
                                       @: ["style" @= "font-weight: bold;"]

                let tag = do Just tc <- getTagType <$> attr "class" "li"
                             [_, tn] <- texts $ "li" // "a"
                             return Tag { tagType = tc, tagName = tn }

                    getTagType "tag-type-general"   = Just General
                    getTagType "tag-type-character" = Just Character
                    getTagType "tag-type-artist"    = Just Artist
                    getTagType "tag-type-copyright" = Just Work

                tags <- chroots ("ul" @: ["id" @= "tag-sidebar"] // "li") tag

                return Post{..}

          rssUrl = "http://gelbooru.com/index.php?page=cooliris"
          postUrl = "http://gelbooru.com/index.php?page=post&s=view&id="

-- ^ Image downloading and storing

fetchImage :: URL -> ExceptT String IO FileInfo
fetchImage url = do
    -- Since we're working with direct links, we can extract the "filename"
    -- from the URL as if it were a path. Kludgy, but works..
    let fileName = takeFileName url
        filePath = imageDir </> fileName
        fileInfo = FileInfo { fileName = T.pack fileName }

    e   <- liftIO $ doesFileExist filePath
    unless e $ download url filePath

    return fileInfo

download :: URL -> FilePath -> ExceptT String IO ()
download url path = do
    res <- liftIO $ curlGetResponse_ url curlOpts
    case res :: CurlResponse_ [(String, String)] LBS.ByteString of
        CurlResponse { respCurlCode = CurlOK, respBody = img }
            -> liftIO $ LBS.writeFile path img

        CurlResponse { respCurlCode = err }
            -> throwE $ show err

-- TODO: thumbnail generation (?), xapian integration, frontend
