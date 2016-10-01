{-# LANGUAGE TemplateHaskell, RecordWildCards, PatternSynonyms, ViewPatterns,
             LambdaCase, TypeFamilies #-}

import Prelude hiding (log)

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Except
import Control.Monad.IO.Class

import Data.Ix (inRange)
import Data.Time
import Data.SafeCopy
import Data.Serialize
import Data.Char
import Data.List (find, isPrefixOf)
import Data.Maybe (maybeToList)

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Network.Curl ( CurlCode(..), CurlOption(..)
                    , CurlResponse_(..), curlGetResponse_ )

import Search.Xapian
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>), takeFileName)
import System.IO

import qualified Search.Xapian.Query.Combinators as Q

import Text.Read (readMaybe)
import Text.HTML.Scalpel hiding (curlOpts, scrape)

-- ^ Global configuration and constants

-- Directories
dbDir       = "/z/booru/"
imageDir    = dbDir </> "images"
xapianDir   = dbDir </> "xapian"

-- Term prefix mapping, for reusable tags
booruPrefix    = "B"
uploaderPrefix = "U"
ratingPrefix   = "R"
tagPrefix      = ""

-- Because we're collecting post data separately from performing the actual
-- downloading, we use tags to signal post state
statePrefix    = "X"
unprocessedTag = "todo"
failedTag      = "failed"

-- Value mapping, for per-document unique identification
siteIDSlot   = 0
scoreSlot    = 1
fileNameSlot = 2
sourceSlot   = 3

-- ^ Types and instances

data Post = Post
    { siteID   :: !Int
    , booru    :: !String
    , rating   :: !Rating
    , uploader :: !Int
    , score    :: !Int
    , source   :: !(Maybe String)
    , fileURL  :: !String
    , fileName :: !String
    , tags     :: ![String]
    } deriving Show

data Rating = Safe | Questionable | Explicit
    deriving (Show, Read)

data SiteScraper = SiteScraper
    { siteName   :: String
    , pageSize   :: !Int
    , scrapePage :: Int -> IO (Maybe [Post])
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

attrRead :: (Selectable s, Read a) => String -> s -> Scraper String a
attrRead a = maybe mzero pure . readMaybe <=< attr a

-- Gelbooru scraper
gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"
          pageSize = 100

          scrapePage page = scrapeURL (apiURL ++ show page) $
                chroots "post" scrapePost

          scrapePost = do
                let post  = "post"
                    booru = siteName

                siteID   <- attrRead "id" post
                fileURL  <- attr     "file_url"   post
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

          apiURL = "http://gelbooru.com/index.php?page=dapi&s=post&q=index&limit=100&pid="

-- ^ Image downloading and storing

{-
fetchImage :: URL -> ExceptT String IO FileInfo
fetchImage url = do
    -- Since we're working with direct links, we can extract the "filename"
    -- from the URL as if it were a path. Kludgy, but works..
    let fileName = takeFileName url
        filePath = imageDir </> fileName

    e   <- liftIO $ doesFileExist filePath
    unless e $ download url filePath

    return FileInfo{..}

download :: URL -> FilePath -> ExceptT String IO ()
download url path = do
    res <- liftIO $ curlGetResponse_ url curlOpts
    case res :: CurlResponse_ [(String, String)] LBS.ByteString of
        CurlResponse { respCurlCode = CurlOK, respBody = img }
            -> liftIO $ LBS.writeFile path img

        CurlResponse { respCurlCode = err }
            -> throwE $ "Error downloading " ++ url ++ ": " ++ show err
-}

-- ^ Xapian database integration

addTag :: Document -> String -> String -> XapianM ()
addTag doc prefix str = addTerm (UTF8.fromString sanitized) doc
    where sanitized = prefix ++ map fix str

          fix ' ' = '_'
          fix c   = toLower c

strVal :: Document -> ValueNumber -> String -> XapianM ()
strVal doc val str = setValue val (UTF8.fromString sanitized) doc
    where sanitized = map toLower str

encVal :: Serialize a => Document -> ValueNumber -> a -> XapianM ()
encVal doc val num = setValue val (encode num) doc

xapianStore :: WritableDatabase db => db -> Post -> XapianM ()
xapianStore db Post{..} = do
    doc <- emptyDocument
    addTag doc statePrefix unprocessedTag
    setData (encode fileURL) doc

    encVal doc siteIDSlot siteID
    encVal doc scoreSlot score
    strVal doc fileNameSlot fileName
    forM_ source $ strVal doc sourceSlot

    addTag doc booruPrefix    $ booru
    addTag doc uploaderPrefix $ show uploader
    addTag doc ratingPrefix   $ show rating
    forM_ tags $ addTag doc tagPrefix

    void $ addDocument db doc

xapianDB :: ExceptT String IO ReadWriteDB
xapianDB = requireRight $ openReadWrite CreateOrOpen xapianDir

runXM :: XapianM a -> ExceptT String IO a
runXM = io . runXapian

-- ^ Front-end routines for the primitives above

scrapeSite :: SiteScraper -> Int -> Int -> Int -> ExceptT String IO ()
scrapeSite SiteScraper{..} offset minID maxID = xapianDB >>= go startPage
    where startPage = offset `div` pageSize

          go :: Int -> ReadWriteDB -> ExceptT String IO ()
          go n db = do
            io.log siteName $ "Scraping page " ++ show n
            res <- io $ scrapePage n

            let process ps = case filter (inRange (minID, maxID) . siteID) ps of
                    [] -> io.log siteName $ "No new posts"
                    ps -> do runXM $ mapM_ (xapianStore db) ps >> commit db
                             mapM_ showPost ps
                             go (n+1) db

            case res of
                Nothing -> throwE "Scraper failed to scrape page"
                Just [] -> throwE "Scraper returned no results"
                Just ps -> process ps


          showPost Post{..} = io.log siteName $ unwords [ show siteID
                                                        , fileURL
                                                        , unwords tags ]

-- ^ Misc helpers

requireJust :: String -> IO (Maybe a) -> ExceptT String IO a
requireJust err = maybe (throwE err) pure <=< liftIO

requireRight :: Show e => IO (Either e a) -> ExceptT String IO a
requireRight = either (throwE . show) pure <=< liftIO

log, logError :: String -> String -> IO ()
log      name msg = hPutStrLn stdout $ "["++name++"] " ++ msg
logError name err = hPutStrLn stderr $ "["++name++"] Error: " ++ err

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

io :: MonadIO m => IO a -> m a
io = liftIO

-- TODO: keep better track of what sites need to be scraped and from where

-- XXX for testing
main = do res <- runExceptT $ scrapeSite gelbooru 0 minBound maxBound
          case res of
            Left e  -> logError "global" e
            Right _ -> return ()
