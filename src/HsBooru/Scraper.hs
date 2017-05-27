-- | Heavy lifting logic for scraping posts and downloading images
module HsBooru.Scraper
    ( download
    , scrapeSite
    , downloadImage
    , storeImages
    , processSite
    ) where

import Prelude hiding (log)

import Control.Exception (try)
import Data.Acid (createCheckpoint)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntervalSet as IS
import qualified Data.Text as T
import qualified Streaming.Prelude as S

import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))
import Streaming
import Pipes.Concurrent

import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian
import HsBooru.Stats

-- * Fetching and scraping

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
download url path = asks fetchURL >>= ($ url) >>= ioCatch . LBS.writeFile path

-- | Checks if an image is already downloaded, and downloads it if it isn't.
requireImage :: FilePath -> URL -> BooruM ()
requireImage fileName fileURL = asks imageDir >>= \case
    Nothing  -> return ()
    Just dir -> do let path = dir </> fileName
                   e <- ioCatch $ doesFileExist path
                   unless e $ download fileURL path

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
storeBatch :: Timer -> Stream (Of Post) BooruM r -> BooruM r
storeBatch t ps = do
    Ctx{..} <- ask
    let f (!r, !s, a) p = ( postState p <> r
                          , postStats p <> s
                          , xapianStore xapianDB p >> a
                          )
    (rec, stats, storeAll) :> r <- S.fold f (mempty, mempty, return ()) id ps
    -- Store them all in the xapian DB first, and only when this succeeds,
    -- the acid db
    runXM  $ txBegin xapianDB >> storeAll >> txCommit xapianDB
    update $ UpdateSites rec

    dt <- io $ measureTimer t
    io.log "db" $ showPostStats stats ++ " => " ++ showPerf (postCount rec) dt
    return r

-- | Stores all received images in batches of size batchSize
storeImages :: Stream (Of Post) BooruM () -> BooruM ()
storeImages ps = do
    n <- asks batchSize
    t <- io newTimer
    let process = fmap (() :>) . storeBatch t
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
    siteState <- query $ GetSite siteName
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
    ioCatch $ createCheckpoint acidDB


-- ** Pipes <-> Streaming boilerplate

streamInput :: MonadIO m => Input a -> Stream (Of a) m ()
streamInput Input{..} = go
    where go = do ma <- io $ atomically recv
                  case ma of Just a  -> S.yield a >> go
                             Nothing -> return ()

sendOutput :: MonadIO m => Output a -> Stream (Of a) m r -> m r
sendOutput Output{..} ps = S.mapM_ (io . atomically . send) ps <* io performGC
