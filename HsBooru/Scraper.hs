-- | Heavy lifting logic for scraping posts and downloading images
module HsBooru.Scraper
    ( URL
    , fetch
    , download
    , scrapeSite
    ) where

import Prelude hiding (log)

import qualified Data.Acid as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntervalSet as IS

import Network.HTTP.Types.Status
import System.Directory (doesFileExist)
import System.FilePath.Posix ((</>))

import HsBooru.Conf
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

type URL = String

-- | Fetch a URL and return its contents as a ByteString
fetch :: Manager -> URL -> ExceptT String IO LBS.ByteString
fetch mgr url = do
    req <- parseRequest url
    res <- ioCatch $ httpLbs req mgr
    let status = responseStatus res
    unless (statusIsSuccessful status) $
        throwE $ "Error downloading " ++ url ++ ": " ++ show status
    return $ responseBody res

-- | Fetch a URL and save it to a file
download :: Manager -> URL -> FilePath -> ExceptT String IO ()
download mgr url path = fetch mgr url >>= ioCatch . LBS.writeFile path

-- | Checks if an image is already downloaded, and downloads it if it isn't
requireImage :: Manager -> Post -> ExceptT String IO ()
requireImage mgr Post{..} = do
    let filePath = imageDir </> fileName
    e <- ioCatch $ doesFileExist filePath
    unless e $ download mgr fileURL filePath

-- | Fetch a single post ID and download the corresponding image. Returns
-- Nothing if we got a reply but the post doesn't seem to exist
fetchID :: SiteScraper -> Manager -> Int -> ExceptT String IO (Maybe Post)
fetchID SiteScraper{..} mgr id = do
    post <- scrapeID mgr id
    forM_ post $ requireImage mgr
    return post

-- | Core scraping loop that runs per-thread
runScraper :: SiteScraper -> Manager -> XapianDB -> InternalDB
           -> PostSet -> ExceptT String IO ()
runScraper _ _ _ _ posts | IS.null posts = return ()
runScraper site@SiteScraper{..} mgr db st posts = do
    let scrapeIDs = take batchSize $ IS.toList posts
    io.log siteName $ "Scraping posts " ++ show scrapeIDs
    res <- forM scrapeIDs $ retry retryCount . fetchID site mgr

    let process (id, post) = case post of
          Nothing -> do
            io.logError siteName $ "Post " ++ show id ++ " deleted!"
            return $ postFailed id

          Just post -> do
            runXM $ do xapianStore db post
            io.log siteName $ showPost post
            return $ postSuccess id

    res <- fmap mconcat . traverse process $ zip scrapeIDs res
    runXM $ commit db
    ioCatch . A.update st $ UpdateSite siteName res
    runScraper site mgr db st $ posts `IS.difference` scrapedMap res

    where showPost Post{..} = unwords [ show siteID, fileURL, unwords tags ]

-- | Fetch all necessary metadata and spawn a bunch of scraper threads
scrapeSite :: SiteScraper -> XapianDB -> InternalDB -> ExceptT String IO ()
scrapeSite site@SiteScraper{..} db st = do
    mgr <- io $ newManager mgrOpts
    siteState <- io $ A.query st (GetSite siteName)
    siteRange <- idRange mgr

    let new = siteRange `IS.difference` scrapedMap siteState
        threads = subdivide new [1 .. threadCount]

    forConcurrentlyE threads $ \(ps, tid) ->
        unless (IS.null ps) $ do
            let range = (IS.findMin ps, IS.findMax ps)
            io.log (siteName ++ "/" ++ show tid) $ show range
            runScraper site mgr db st ps

    ioCatch $ A.createCheckpoint st
