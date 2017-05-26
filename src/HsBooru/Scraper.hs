-- | Heavy lifting logic for scraping posts and downloading images
module HsBooru.Scraper
    ( URL
    , fetch
    , download
    , scrapeSite
    , downloadImage
    , storeImages
    , processSite
    ) where

import Prelude hiding (log)

import Control.Exception (try)
import qualified Data.Acid as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntervalSet as IS
import qualified Data.Text as T
import qualified Streaming.Prelude as S

import Network.HTTP.Types.Status
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import Streaming

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
scrapeSite :: SiteScraper -> Manager -> PostSet -> Stream (Of Post) BooruM ()
scrapeSite SiteScraper{..} mgr = go where
    go posts | IS.null posts = return ()
    go posts = do
        let id = IS.findMin posts
        lift (tryScrape id) >>= S.yield
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


-- * Database interaction and finalization

-- | Store a batch of images in the database, throwing on any failure.
-- The batch will be committed atomically, i.e. all-or-nothing.
storeBatch :: (XapianDB, InternalDB) -> Stream (Of Post) BooruM r -> BooruM r
storeBatch (db, st) ps = do
    let update (!s, a) p = (postState p <> s, xapianStore db p >> a)
    (rec, storeAll) :> r <- S.fold update (mempty, return ()) id ps
    -- Store them all in the xapian DB first, and only when this succeeds,
    -- the acid db
    runXM $ txBegin db >> storeAll >> txCommit db
    io . A.update st $ UpdateSites rec
    io.log "db" $ "Committed " ++ show (postCount rec) ++ " post(s) to the DB"
    return r

-- | Stores all received images in batches of size batchSize
storeImages :: (XapianDB, InternalDB) -> Stream (Of Post) BooruM () -> BooruM ()
storeImages dbs ps = S.effects . S.mapped process $ chunksOf batchSize ps
    where process ps' = (() :>) <$> storeBatch dbs ps'


-- * Overall processing logic

inspectPost :: Post -> BooruM ()
inspectPost p = io $ case p of
    PostFailure{..} -> logError postSite $ showID p ++ "failed: " ++ reason
    PostDeleted{..} -> log      postSite $ showID p ++ "{deleted}"
    PostSuccess{..} -> log      postSite $ showID p ++ T.unpack (T.unwords tags)
    where showID p = show (siteID p) ++ " >>> "

processSite :: SiteScraper -> (XapianDB, InternalDB) -> BooruM ()
processSite s@SiteScraper{..} (db, st) = do
    mgr <- io $ newManager mgrOpts
    siteState <- io $ A.query st (GetSite siteName)
    siteRange <- idRange mgr

    let known = scrapedMap siteState
        new = siteRange `IS.difference` known
        threads = subdivide new [1 .. threadCount]

    io.log siteName $ "Have: " ++ show (IS.size known)
                   ++    " / " ++ show (IS.size siteRange)
                   ++ " New: " ++ show (IS.size new)

    let posts = S.mapM (downloadImage mgr) $ scrapeSite s mgr new
    storeImages (db, st) $ S.chain inspectPost posts
    ioCatch $ A.createCheckpoint st
