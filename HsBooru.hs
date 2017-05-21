{-# LANGUAGE TemplateHaskell, RecordWildCards, PatternSynonyms, ViewPatterns,
             LambdaCase, TypeFamilies, TypeSynonymInstances, FlexibleInstances,
             ScopedTypeVariables, OverloadedStrings #-}

import Prelude hiding (log)

import Control.Applicative
import Control.Concurrent.Async (forConcurrently)
import Control.Exception
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.State (modify, gets)
import Control.Monad.Reader (ask)

import Data.Default
import Data.Ix (inRange)
import Data.Time
import Data.SafeCopy
import Data.Char
import Data.List (find, isPrefixOf)
import Data.Maybe (maybeToList)

import qualified Data.Acid as A
import qualified Data.Map as M
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import qualified Network.Curl as Curl
import Network.Curl ( Curl(..), CurlCode(..), CurlOption(..)
                    , CurlResponse_(..) )

import Search.Xapian
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeFileName, takeExtension)
import System.IO

import qualified Search.Xapian.Query.Combinators as Q

import Text.Read (readMaybe)
import Text.HTML.Scalpel hiding (curlOpts, scrape)

-- ^ Global configuration and constants

-- Directories
dbDir       = "/booru/"
imageDir    = dbDir </> "images"
xapianDir   = dbDir </> "xapian"
acidDir     = dbDir </> "acid"

-- Some misc tuning options
curlOpts    = [ CurlFollowLocation True, CurlConnectTimeout 100, CurlTimeout 600 ]
retryCount  = 5
updateBatch = 10
pageBatch   = 5

-- Term prefix mapping, for reusable tags
statePrefix    = "X"
booruPrefix    = "B"
uploaderPrefix = "U"
ratingPrefix   = "R"
extPrefix      = "E"
filePrefix     = "F"
tagPrefix      = ""

-- Because we're collecting post data separately from performing the actual
-- downloading, we use tags to signal post state
unprocessedTag = mkStateTag "todo"
failedTag      = mkStateTag "failed"

mkStateTag :: String -> BS.ByteString
mkStateTag tag = UTF8.fromString $ statePrefix ++ tag

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

-- Can't be a newtype due to overly strict nominal role constraints on SafeCopy
type ScraperState = M.Map String Int

-- ^ acid-state integration

acidDB :: IO (A.AcidState ScraperState)
acidDB = A.openLocalStateFrom acidDir M.empty

activeSites :: A.Query ScraperState [String]
activeSites = M.keys <$> ask

recordMax :: String -> Int -> A.Update ScraperState ()
recordMax k = modify . M.insertWith max k

requireMax :: String -> A.Update ScraperState Int
requireMax k = do
    v <- gets $ M.lookup k
    case v of
        Nothing -> 0 <$ modify (M.insert k 0)
        Just v' -> return v'

A.makeAcidic ''ScraperState ['activeSites, 'recordMax, 'requireMax]

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

attrRead :: Read a => String -> Selector -> Scraper String a
attrRead a = maybe mzero pure . readMaybe <=< attr a

scrapers :: [SiteScraper]
scrapers = [ gelbooru ]

-- Gelbooru scraper
gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"
          pageSize = 100

          scrapePage page = do
                let pageURL = apiURL ++ show page
                log siteName $ "Requesting " ++ pageURL
                scrapeURLWithOpts curlOpts pageURL $ chroots "post" scrapePost
                                                  <|> pure []

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

          apiURL = "http://gelbooru.com/index.php?page=dapi&s=post"++
                   "&q=index&limit="++show pageSize++"&pid="

-- ^ Image downloading and storing

requireImage :: Curl -> URL -> String -> ExceptT String IO ()
requireImage curl url fileName = do
    let filePath = imageDir </> fileName
    e <- liftIO $ doesFileExist filePath
    unless e $ download curl url filePath

download :: Curl -> URL -> FilePath -> ExceptT String IO ()
download curl url path = do
    res <- ioCatch $ Curl.do_curl_ curl url curlOpts
    case res :: CurlResponse_ [(String, String)] LBS.ByteString of
        CurlResponse { respCurlCode = CurlOK, respBody = img }
            -> ioCatch $ LBS.writeFile path img

        CurlResponse { respCurlCode = err }
            -> throwE $ "Error downloading " ++ url ++ ": " ++ show err

-- ^ Xapian database integration

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

xapianStore :: WritableDatabase db => db -> Post -> XapianM ()
xapianStore db Post{..} = do
    doc <- emptyDocument
    addTerm unprocessedTag doc
    setData (UTF8.fromString fileURL) doc

    encVal doc siteIDSlot siteID
    encVal doc scoreSlot score
    strVal doc fileNameSlot fileName
    forM_ source $ strVal doc sourceSlot

    addTag doc booruPrefix    $ booru
    addTag doc uploaderPrefix $ show uploader
    addTag doc ratingPrefix   $ show rating
    addTag doc filePrefix     $ fileName
    addTag doc extPrefix      $ drop 1 (takeExtension fileName)
    forM_ tags $ addTag doc tagPrefix

    void $ addDocument db doc

xapianDB :: ExceptT String IO ReadWriteDB
xapianDB = requireRight $ openReadWrite CreateOrOpen xapianDir

runXM :: XapianM a -> ExceptT String IO a
runXM = ioCatch . runXapian

-- ^ Mid-level wrappers for the above primitives

data ScrapeRange = ScrapeRange
    { startPage :: !Int
    , minID :: !Int
    , maxID :: !Int
    }

instance Default ScrapeRange where
    def = ScrapeRange { startPage = 0, minID = minBound, maxID = maxBound }

scrapeSiteWith :: SiteScraper -> ReadWriteDB -> A.AcidState ScraperState
               -> ScrapeRange -> ExceptT String IO ()

scrapeSiteWith SiteScraper{..} db st ScrapeRange{..} = go startPage
    where go :: Int -> ExceptT String IO ()
          go n = do
            let scrapeRange = [n .. n + pageBatch - 1]
            io.log siteName $ "Scraping pages " ++ show scrapeRange
            res <- io.forConcurrently scrapeRange $ retry retryCount . scrapePage

            let process ps = case filter (inRange (minID, maxID) . siteID) ps of
                    [] -> io.log siteName $ "No new posts"
                    ps -> do runXM $ mapM_ (xapianStore db) ps >> commit db
                             let m = maximum $ map siteID ps
                             ioCatch $ A.update st (RecordMax siteName m)
                             ioCatch $ A.createCheckpoint st
                             mapM_ showPost ps
                             go (n+pageBatch)

            case sequence res of
                Nothing -> throwE "Scraper failed to scrape page"
                Just [] -> throwE "Scraper returned no results"
                Just ps -> process $ concat ps


          showPost Post{..} = io.log siteName $ unwords [ show siteID
                                                        , fileURL
                                                        , unwords tags ]

downloadDoc :: Curl -> ReadWriteDB -> Document -> XapianM ()
downloadDoc curl db doc = do
    url       <- UTF8.toString <$> getData doc
    fileName  <- UTF8.toString <$> getValue fileNameSlot doc

    res <- io.runExceptT $ requireImage curl url fileName
    case res of
        Right _ -> do io $ log "dl" url
        Left  e -> do io $ logError url e
                      addTerm failedTag doc

    delTerm unprocessedTag doc
    replaceDocument db (docId doc) doc

-- ^ High-level commands to perform

scrapeSite :: ReadWriteDB -> A.AcidState ScraperState -> SiteScraper -> IO ()
scrapeSite db st site@SiteScraper{..} = do
    lastMax <- io $ A.update st (RequireMax siteName)
    let range = def { minID = lastMax + 1 }
    res <- runExceptT $ scrapeSiteWith site db st range
    case res of
        Left e  -> logError siteName e
        Right _ -> return ()

updateImages :: ReadWriteDB -> IO ()
updateImages db = do
    curl <- Curl.initialize
    let limit = QueryRange { rangeOffset = 0, rangeSize = updateBatch }

        -- Loops until no more unprocessed documents
        go = do (_, ds) <- search db (query unprocessedTag) limit
                case ds of [] -> return ()
                           ds -> do forM_ ds $ downloadDoc curl db
                                    commit db
                                    go

    runXapian go

main :: IO ()
main = do
    args <- getArgs
    st   <- acidDB
    db   <- either error id <$> runExceptT xapianDB

    -- When running without a parameter, scrape the previously active
    -- sites. Otherwise, scrape the names sites
    sites <- case args of
                [] -> A.query st ActiveSites
                as -> return as

    forM_ sites $ \sn -> case find (\s -> siteName s == sn) scrapers of
        Just s -> scrapeSite db st s
        Nothing -> logError sn "No scraper found for this site!"

    A.closeAcidState st

    log "general" "Done scraping, proceeding to download new images"
    Curl.withCurlDo $ updateImages db

-- ^ Misc helpers

requireJust :: String -> IO (Maybe a) -> ExceptT String IO a
requireJust err = maybe (throwE err) pure <=< io

requireRight :: Show e => IO (Either e a) -> ExceptT String IO a
requireRight = either (throwE . show) pure <=< io

log, logError :: String -> String -> IO ()
log      name msg = hPutStrLn' stdout $ "["++name++"] " ++ msg
logError name err = hPutStrLn' stderr $ "["++name++"] Error: " ++ err

hPutStrLn' :: Handle -> String -> IO ()
hPutStrLn' h str = do
    t <- getZonedTime
    hPutStrLn h $ show t ++ " " ++ str

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)

io :: MonadIO m => IO a -> m a
io = liftIO

ioCatch :: IO a -> ExceptT String IO a
ioCatch act = ExceptT $ try act <&> lmap (show :: IOException -> String)

lmap :: (a -> b) -> Either a x -> Either b x
lmap f (Left  x) = Left (f x)
lmap _ (Right x) = Right x

retry :: Int -> IO (Maybe a) -> IO (Maybe a)
retry 0 _   = return Nothing
retry n act = do
    res <- act
    case res of
        Just r  -> return res
        Nothing -> retry (n-1) act
