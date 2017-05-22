{-# LANGUAGE TemplateHaskell, RecordWildCards, PatternSynonyms, ViewPatterns,
             LambdaCase, TypeFamilies, TypeSynonymInstances, FlexibleInstances,
             ScopedTypeVariables, OverloadedStrings #-}

import Prelude hiding (log)

import Control.Applicative
import Control.Concurrent.Async (Concurrently(..), mapConcurrently)
import Control.Exception
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.State (modify, gets)
import Control.Monad.Reader (asks)

import Data.Default
import Data.Ix (inRange)
import Data.Time
import Data.SafeCopy
import Data.Char
import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)

import qualified Data.Acid as A
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntervalSet as IS
import qualified Data.Map as M
import qualified Data.Text as T

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status

import Search.Xapian
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath.Posix ((</>), takeFileName, takeExtension)
import System.IO

import qualified Search.Xapian.Query.Combinators as Q

import Text.Read (readMaybe)
import Text.HTML.Scalpel.Core hiding (scrape)

-- ^ Global configuration and constants

-- Directories
dbDir       = "/booru/"
imageDir    = dbDir </> "images"
xapianDir   = dbDir </> "xapian"
acidDir     = dbDir </> "acid"

-- Some misc tuning options
mgrOpts    = tlsManagerSettings { managerConnCount = postBatch }
retryCount = 5
postBatch  = 10

-- Term prefix mapping, for reusable tags
booruPrefix    = "B"
siteIDPrefix   = "I"
uploaderPrefix = "U"
ratingPrefix   = "R"
extPrefix      = "E"
filePrefix     = "F"
tagPrefix      = ""

-- Value mapping, for per-document unique identification
siteIDSlot   = 0
scoreSlot    = 1
fileNameSlot = 2
fileURLSlot  = 3
sourceSlot   = 4

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
    { siteName :: String
    , idRange  :: Manager -> ExceptT String IO IS.IntSet
    , scrapeID :: Manager -> Int -> ExceptT String IO (Maybe Post)
    }

-- ^ Scraper internal state and database

type PostSet = IS.IntSet

-- Invariant: successMap and failedMap are disjoint
data SiteState = SiteState
    { successMap :: !PostSet
    , failedMap  :: !PostSet
    }

instance Monoid SiteState where
    mempty = def
    mappend (SiteState ca fa) (SiteState cb fb) = SiteState{..}
        -- Success overrides failure
        where successMap = IS.union ca cb
              failedMap  = IS.union fa fb `IS.difference` successMap

instance Default SiteState where
    def = SiteState IS.empty IS.empty

postSuccess, postFailed :: Int -> SiteState
postSuccess id = def { successMap = IS.singleton id }
postFailed  id = def { failedMap  = IS.singleton id }

allPosts :: SiteState -> PostSet
allPosts SiteState{..} = successMap `IS.union` failedMap

-- Can't be a newtype due to overly strict nominal role constraints on SafeCopy
type ScraperState = M.Map String SiteState

-- ^ acid-state integration

deriveSafeCopy 0 'base ''IS.IntSet
deriveSafeCopy 0 'base ''SiteState

acidDB :: IO (A.AcidState ScraperState)
acidDB = A.openLocalStateFrom acidDir M.empty

activeSites :: A.Query ScraperState [String]
activeSites = asks M.keys

getSite :: String -> A.Query ScraperState SiteState
getSite k = asks $ fromMaybe def . M.lookup k

updateSite :: String -> SiteState -> A.Update ScraperState ()
updateSite k = modify . M.insertWith mappend k

A.makeAcidic ''ScraperState ['activeSites, 'getSite, 'updateSite]

-- ^ Scraper definitions

type URL = String

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

first :: Selector -> Scraper String a -> Scraper String (Maybe a)
first s = fmap listToMaybe . chroots s

scrape :: Manager -> URL -> Scraper String a -> ExceptT String IO a
scrape mgr url s = do
    io.log "http" $ "Scraping " ++ url
    body <- LUTF8.toString <$> fetch mgr url
    maybe (throwE "Scraper returned no results") return $ scrapeStringLike body s

scrapers :: [SiteScraper]
scrapers = [ gelbooru ]

-- Gelbooru scraper
gelbooru :: SiteScraper
gelbooru = SiteScraper{..}
    where siteName = "gelbooru"
          apiURL = "https://gelbooru.com/index.php?page=dapi&s=post&q=index"

          idRange mgr = do
                let indexURL = apiURL ++ "&limit=0"
                c <- scrape mgr indexURL $ attrRead "count" "posts"
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

-- ^ Image downloading and storing

requireImage :: Manager -> Post -> ExceptT String IO ()
requireImage mgr Post{..} = do
    let filePath = imageDir </> fileName
    e <- liftIO $ doesFileExist filePath
    unless e $ download mgr fileURL filePath

fetch :: Manager -> URL -> ExceptT String IO LBS.ByteString
fetch mgr url = do
    req <- parseRequest url
    res <- ioCatch $ httpLbs req mgr
    let status = responseStatus res
    unless (statusIsSuccessful status) $
        throwE $ "Error downloading " ++ url ++ ": " ++ show status
    return $ responseBody res

download :: Manager -> URL -> FilePath -> ExceptT String IO ()
download mgr url path = fetch mgr url >>= ioCatch . LBS.writeFile path

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

xapianDB :: ExceptT String IO ReadWriteDB
xapianDB = requireRight $ openReadWrite CreateOrOpen xapianDir

runXM :: XapianM a -> ExceptT String IO a
runXM = ioCatch . runXapian

-- ^ Mid-level wrappers for the above primitives

fetchID :: SiteScraper -> Manager -> Int -> ExceptT String IO (Maybe Post)
fetchID SiteScraper{..} mgr id = do
    post <- scrapeID mgr id
    forM_ post $ requireImage mgr
    return post

scrapeSiteWith :: SiteScraper -> Manager -> ReadWriteDB -> A.AcidState ScraperState
               -> ExceptT String IO ()

scrapeSiteWith site@SiteScraper{..} mgr db st = do
    siteState <- io $ A.query st (GetSite siteName)
    siteRange <- idRange mgr

    let new = siteRange `IS.difference` allPosts siteState
    io.log siteName $ "New post range: " ++ show (IS.findMin new, IS.findMax new)
    go new

    ioCatch $ A.createCheckpoint st

    where go :: PostSet -> ExceptT String IO ()
          go posts | IS.null posts = return ()

          go posts = do
            let scrapeIDs = take postBatch $ IS.toList posts
            io.log siteName $ "Scraping posts " ++ show scrapeIDs
            res <- forConcurrentlyE scrapeIDs $ retry retryCount . fetchID site mgr

            let process (id, post) = case post of
                  Nothing -> do
                    io.logError siteName $ "Post " ++ show id ++ " deleted!"
                    return $ postFailed id

                  Just post -> do
                    io.log siteName $ showPost post
                    runXM  $ xapianStore db post
                    return $ postSuccess id

            res <- fmap mconcat . traverse process $ zip scrapeIDs res
            runXM $ commit db
            ioCatch . A.update st $ UpdateSite siteName res
            go $ posts `IS.difference` allPosts res

          showPost Post{..} = unwords [ show siteID, fileURL, unwords tags ]

-- ^ High-level commands to perform

scrapeSite :: ReadWriteDB -> A.AcidState ScraperState -> SiteScraper -> IO ()
scrapeSite db st site@SiteScraper{..} = do
    mgr <- newManager mgrOpts
    res <- runExceptT $ scrapeSiteWith site mgr db st
    case res of
        Left e  -> logError siteName e
        Right _ -> return ()

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
    log "general" "Done scraping,"

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

retry :: MonadPlus m => Int -> m a -> m a
retry n = msum . replicate n

forConcurrentlyE :: Traversable t => t a -> (a -> ExceptT e IO b) -> ExceptT e IO (t b)
forConcurrentlyE ts f = recombine $ distribute
    where distribute = mapConcurrently (runExceptT . f) ts
          recombine  = ExceptT . fmap sequence
