{-# LANGUAGE OverloadedStrings #-}
-- | Module holding site-specific scrapers
module HsBooru.Sites (scrapers) where

import Prelude hiding (log)

import Data.Char (isNumber)
import Text.HTML.Scalpel.Core hiding (scrape)
import Text.Read (readMaybe)
import System.FilePath.Posix (takeFileName)

import qualified Data.ByteString.Lazy.UTF8 as UTF8
import qualified Data.IntervalSet as IS

import HsBooru.Scraper
import HsBooru.Types
import HsBooru.Util

-- Helper functions

-- | Drops all non-numerics and then tries to read what's left as a number
extractID :: URL -> Maybe Int
extractID = readMaybe . dropWhile (not . isNumber)

-- | Tries stripping a prefix from a string
chomp :: String -> String -> Maybe String
chomp [] ys = Just ys
chomp _  [] = Nothing
chomp (x:xs) (y:ys)
    | x == y    = chomp xs ys
    | otherwise = Nothing

-- | Tries reading the contents of an attribute as an arbitrary Read type
attrRead :: Read a => String -> Selector -> Scraper String a
attrRead a = maybe mzero pure . readMaybe <=< attr a

-- | Like `chroots`, but only returns the first match
first :: Selector -> Scraper String a -> Scraper String (Maybe a)
first s = fmap listToMaybe . chroots s

-- | Helper function to run a scraper on a URL
scrape :: Manager -> URL -> Scraper String a -> ExceptT String IO a
scrape mgr url s = do
    io.log "http" $ "Scraping " ++ url
    body <- UTF8.toString <$> fetch mgr url
    maybe (throwE "Scraper returned no results") return $ scrapeStringLike body s

-- | List of supported website scrapers
scrapers :: [SiteScraper]
scrapers = [ gelbooru ]

-- Website-specific scrapers

gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"
          apiURL = "https://gelbooru.com/index.php?page=dapi&s=post&q=index"

          idRange mgr = do
                -- To get the highest ID, we fetch a single post and read out
                -- its id attribute. This will always be the newest
                let indexURL = apiURL ++ "&limit=1"
                c <- scrape mgr indexURL $ attrRead "id" "post"
                return $ IS.interval 1 c

          scrapeID mgr id = do
                let postURL = apiURL ++ "&id=" ++ show id
                scrape mgr postURL $ first "post" scrapePost

          scrapePost = do
                let post  = "post"
                    booru = siteName

                siteID   <- attrRead "id" post
                fileURL  <- ("http:" ++) <$> attr "file_url" post
                uploader <- attrRead "creator_id" post
                score    <- attrRead "score"      post
                tags     <- attr "tags"   post <&> words
                rating   <- attr "rating" post <&> \case "s" -> Safe
                                                         "q" -> Questionable
                                                         "e" -> Explicit

                source   <- attr "source" post <&> \case ""  -> Nothing
                                                         src -> Just src


                -- Since we're working with direct links, we can extract the
                -- "filename" from the URL as if it were a path. Kludgy, but
                -- works..
                let fileName = takeFileName fileURL

                return Post{..}
