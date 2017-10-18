{-# LANGUAGE OverloadedStrings #-}
-- | Module holding site-specific scrapers
module HsBooru.Sites
    ( scrapers
    , findSite
    -- ** Re-exported for convenience (ghci etc.)
    , gelbooru
    ) where

import Prelude hiding (log)

import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Format

import Text.HTML.Scalpel.Core hiding (scrape)
import Text.Read (readMaybe)
import System.FilePath.Posix (takeFileName)

import qualified Data.IntervalSet as IS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import HsBooru.Types
import HsBooru.Util

-- Helper functions

-- | Tries reading the contents of an attribute as an arbitrary Read type
attrRead :: Read a => String -> Selector -> Scraper LT.Text a
attrRead a = maybe mzero pure . readMaybe . LT.unpack <=< attr a

attrText :: String -> Selector -> Scraper LT.Text T.Text
attrText a = fmap LT.toStrict . attr a

-- | Like `chroots`, but only returns the first match
first :: Selector -> Scraper LT.Text a -> Scraper LT.Text (Maybe a)
first s = fmap listToMaybe . chroots s

-- | Helper function to run a scraper on a URL
scrape :: URL -> Scraper LT.Text a -> BooruM a
scrape url s = do
    Ctx{..} <- ask
    when verbose $ io.log "http" $ "Attempting to scrape " ++ url
    body <- decodeUtf8 <$> fetchURL url
    maybe (throwB Temporary "Scraper returned no results") return $
        scrapeStringLike body s

-- | List of supported website scrapers
scrapers :: [SiteScraper]
scrapers = [ gelbooru ]

findSite :: String -> Maybe SiteScraper
findSite s = find (\ss -> siteName ss == s) scrapers

-- Website-specific scrapers

gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"
          postSite = siteName
          apiURL   = "https://gelbooru.com/index.php?page=dapi&s=post&q=index"

          idRange = do
                -- To get the highest ID, we fetch a single post and read out
                -- its id attribute. This will always be the newest
                let indexURL = apiURL ++ "&limit=1"
                c <- scrape indexURL $ attrRead "id" "post"
                return $ IS.interval 1 c

          scrapeID siteID = do
                let postURL = apiURL ++ "&id=" ++ show siteID
                    count   = attrRead "count" "posts"
                    post    = first "post" scrapePost

                res <- scrape postURL $ liftM2 (,) count post
                return $ case res of
                    (1, Just p)  -> p
                    (0, Nothing) -> PostDeleted{..}
                    (_, _)       -> PostFailure{reason   = "Unknown failure",
                                                failType = Temporary, ..}

          scrapePost = do
                let post = "post"
                siteID   <- attrRead "id" post
                uploader <- attrRead "creator_id" post <|> pure 0
                score    <- attrRead "score" post
                tags     <- attrText "tags" post <&> T.words
                fileURL  <- fixURL <$> attrText "file_url" post
                rating   <- attr "rating" post >>= \case
                                "s" -> pure Safe
                                "q" -> pure Questionable
                                "e" -> pure Explicit
                                _   -> fail "Failed parsing rating"
                source   <- attr "source" post <&> \case
                                ""  -> Nothing
                                src -> Just (LT.toStrict src)
                let parseTime = parseTimeM False defaultTimeLocale "%c"
                uploaded <- parseTime =<< LT.unpack <$> attr "created_at" post


                -- Since we're working with direct links, we can extract the
                -- "filename" from the URL as if it were a path. Kludgy, but
                -- works..
                let fileName = T.pack $ takeFileName (T.unpack fileURL)

                return PostSuccess{..}

          fixURL url | "http" `T.isPrefixOf` url = url
                     | otherwise = "http:" <> url
