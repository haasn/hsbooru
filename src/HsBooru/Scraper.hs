-- | Heavy lifting logic for scraping posts and downloading images
module HsBooru.Scraper
    ( URL
    , fetch
    , download
    , scrapeSite
    , downloadImages
    , storeImages
    , processSite
    ) where

import Prelude hiding (log)

import Control.Exception (try)
import qualified Data.Acid as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntervalSet as IS
import qualified Data.Text as T
import qualified Pipes.Prelude as P

import Network.HTTP.Types.Status
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import Pipes

import HsBooru.Conf
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

-- * Fetching and scraping

type URL = String

-- | Fetch a URL and return its contents as a ByteString. Retries a number
-- of times according to retryCount
fetch :: Manager -> URL -> BooruM LBS.ByteString
fetch mgr url = retry retryCount $ do
    req <- io $ parseRequest url
    res <- requireRight . tryHttp $ httpLbs req mgr
    let status = responseStatus res
    unless (statusIsSuccessful status) $
        throwB $ "Error downloading " ++ url ++ ": " ++ show status
    return $ responseBody res
  where tryHttp :: IO a -> IO (Either HttpException a)
        tryHttp = try

-- | Run a scraper on a website and range, returning a stream of Posts.
scrapeSite :: SiteScraper -> Manager -> PostSet -> Producer Post BooruM ()
scrapeSite SiteScraper{..} mgr = go where
    go posts | IS.null posts = return ()
    go posts = do
        let id = IS.findMin posts
        lift (tryScrape id) >>= yield
        go $ IS.delete id posts

    tryScrape siteID = scrapeID mgr siteID `catchB`
        \reason -> return PostFailure{postSite = siteName, ..}

-- * Downloading and storing

-- | Fetch a URL and save it to a file
download :: Manager -> URL -> FilePath -> BooruM ()
download mgr url path = fetch mgr url >>= ioCatch . LBS.writeFile path

-- | Checks if an image is already downloaded, and downloads it if it isn't.
requireImage :: Manager -> FilePath -> URL -> BooruM ()
requireImage mgr fileName fileURL = do
    let filePath = imageDir </> fileName
    e <- ioCatch $ doesFileExist filePath
    unless e $ download mgr fileURL filePath

-- | Attempt to download an image, transforming PostSuccess into PostFailure on
-- failure
downloadImage :: Manager -> Post -> BooruM Post
downloadImage _   p@PostDeleted{}   = return p
downloadImage _   p@PostFailure{}   = return p
downloadImage mgr p@PostSuccess{..} = tryDL `catchB` \reason -> return PostFailure{..}
    where tryDL = p <$ requireImage mgr (T.unpack fileName) (T.unpack fileURL)

-- | Download all images, transforming PostSuccess into PostFailure where the
-- download failed
downloadImages :: Manager -> Pipe Post Post BooruM r
downloadImages = P.mapM . downloadImage


-- * Database interaction and finalization

-- | Store a batch of images in the database, throwing on any failure.
-- The batch will be committed atomically, i.e. all-or-nothing.
storeBatch :: (XapianDB, InternalDB) -> [Post] -> BooruM ()
storeBatch (db, st) ps = do
    -- Store all of the posts in the xapian DB as a transaction
    runXM $ do txBegin db
               mapM_ (xapianStore db) ps
               txCommit db
    -- Assuming this succeeded, record all of the IDs into the acid state
    io . A.update st . UpdateSites $ foldMap postState ps

-- | A consumer that stores all received images one by one.
-- Note: Very inefficient
storeImages :: (XapianDB, InternalDB) -> Consumer Post BooruM ()
storeImages dbs = P.mapM_ $ \p -> storeBatch dbs [p]


-- * Overall processing logic
processSite :: SiteScraper -> (XapianDB, InternalDB) -> BooruM ()
processSite s@SiteScraper{..} (db, st) = do
    mgr <- io $ newManager mgrOpts
    siteState <- io $ A.query st (GetSite siteName)
    siteRange <- idRange mgr

    let known = scrapedMap siteState
        new = siteRange `IS.difference` known
        threads = subdivide new [1 .. threadCount]

    io.log siteName $ "Have: " ++ show (IS.size known)
                   ++      "/" ++ show (IS.size siteRange)
                   ++ " New: " ++ show (IS.size new)

    runEffect $ scrapeSite s mgr new >-> downloadImages mgr >-> storeImages (db, st)
    ioCatch   $ A.createCheckpoint st
