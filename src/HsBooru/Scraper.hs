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
import Pipes.Concurrent

import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

-- * Fetching and scraping

type URL = String

-- | Fetch a URL and return its contents as a ByteString. Retries a number
-- of times according to retryCount
fetch :: URL -> BooruM LBS.ByteString
fetch url = ask >>= \Ctx{..} -> retry retryCount $ do
    req <- io $ parseRequest url
    res <- requireRight . tryHttp $ httpLbs req manager
    let status = responseStatus res
    unless (statusIsSuccessful status) $
        throwB $ "Error downloading " ++ url ++ ": " ++ show status
    return $ responseBody res
  where tryHttp :: IO a -> IO (Either HttpException a)
        tryHttp = try

-- | Run a scraper on a website and range, returning a stream of Posts.
scrapeSite :: SiteScraper -> PostSet -> Stream (Of Post) BooruM ()
scrapeSite SiteScraper{..} = go where
    go posts | IS.null posts = return ()
    go posts = do
        let id = IS.findMin posts
        lift (tryScrape id) >>= S.yield
        go $ IS.delete id posts

    tryScrape siteID = (scrapeID siteID >>= applyFilter) `catchB`
        \reason -> return PostFailure{postSite = siteName, ..}

applyFilter :: Post -> BooruM Post
applyFilter p@PostSuccess{..} = do
    Ctx{..} <- ask
    when (length tags < minTagCount) $ throwB "Post has too few tags"
    return p

applyFilter p = return p

-- * Downloading and storing

-- | Fetch a URL and save it to a file
download :: URL -> FilePath -> BooruM ()
download url path = fetch url >>= ioCatch . LBS.writeFile path

-- | Checks if an image is already downloaded, and downloads it if it isn't.
requireImage :: FilePath -> URL -> BooruM ()
requireImage fileName fileURL = do
    Ctx{..} <- ask
    let filePath = imageDir </> fileName
    e <- ioCatch $ doesFileExist filePath
    unless e $ download fileURL filePath

-- | Attempt to download an image, transforming PostSuccess into PostFailure on
-- failure
downloadImage :: Post -> BooruM Post
downloadImage p@PostDeleted{}   = return p
downloadImage p@PostFailure{}   = return p
downloadImage p@PostSuccess{..} = tryDL `catchB` \reason -> return PostFailure{..}
    where tryDL = p <$ requireImage (T.unpack fileName) (T.unpack fileURL)


-- * Database interaction and finalization

-- | Store a batch of images in the database, throwing on any failure.
-- The batch will be committed atomically, i.e. all-or-nothing.
storeBatch :: Stream (Of Post) BooruM r -> BooruM r
storeBatch ps = do
    Ctx{..} <- ask
    let update (!s, a) p = (postState p <> s, xapianStore xapianDB p >> a)
    (rec, storeAll) :> r <- S.fold update (mempty, return ()) id ps
    -- Store them all in the xapian DB first, and only when this succeeds,
    -- the acid db
    runXM $ txBegin xapianDB >> storeAll >> txCommit xapianDB
    io . A.update acidDB $ UpdateSites rec
    io.log "db" $ "Committed " ++ show (postCount rec) ++ " post(s) to the DB"
    return r

-- | Stores all received images in batches of size batchSize
storeImages :: Stream (Of Post) BooruM () -> BooruM ()
storeImages ps = do
    n <- asks batchSize
    let process = fmap (() :>) . storeBatch
    S.effects . S.mapped process $ chunksOf n ps


-- * Overall processing logic

inspectPost :: Post -> BooruM ()
inspectPost p = ask >>= \Ctx{..} -> io $ case p of
    PostFailure{..} -> logError postSite $ showID p ++ "failed: " ++ reason
    _ | not verbose -> return ()
    PostDeleted{..} -> log      postSite $ showID p ++ "{deleted}"
    PostSuccess{..} -> log      postSite $ showID p ++ T.unpack (T.unwords tags)
    where showID p = show (siteID p) ++ " >>> "

processSite :: SiteScraper -> BooruM ()
processSite s@SiteScraper{..} = do
    Ctx{..}   <- ask
    siteState <- io $ A.query acidDB (GetSite siteName)
    siteRange <- idRange

    let known   = scrapedMap siteState
        new     = siteRange `IS.difference` known
        threads = subdivide new [1 .. threadCount]

    io.log siteName $ "Have: " ++ show (IS.size known)
                   ++    " / " ++ show (IS.size siteRange)
                   ++ " New: " ++ show (IS.size new)

    -- Spawn a concurrent mailbox and connects the post stream with the
    -- storeImages consumer via it
    (out, inp) <- io.spawn $ bounded batchSize

    -- Upstream
    let runThread  = sendOutput out
                   . S.chain inspectPost
                   . S.mapM downloadImage
                   . scrapeSite s

    let threads = subdivide new $ replicate threadCount runThread
    forM_ threads $ \(r, f) -> lower (f r) >>= io . forkIO . void

    -- Downstream
    storeImages $ streamInput inp
    ioCatch $ A.createCheckpoint acidDB


-- ** Pipes <-> Streaming boilerplate

streamInput :: MonadIO m => Input a -> Stream (Of a) m ()
streamInput Input{..} = go
    where go = do ma <- io $ atomically recv
                  case ma of Just a  -> S.yield a >> go
                             Nothing -> return ()

sendOutput :: MonadIO m => Output a -> Stream (Of a) m r -> m r
sendOutput Output{..} ps = S.mapM_ (io . atomically . send) ps <* io performGC
