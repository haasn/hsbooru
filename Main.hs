{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)

import Data.Monoid ((<>))
import Options.Applicative

import GHC.Conc (getNumProcessors)
import Control.Concurrent
import System.Directory (createDirectoryIfMissing)
import System.FilePath

import qualified Data.Acid as A
import qualified Data.Text as T

import HsBooru.Scraper
import HsBooru.Sites
import HsBooru.Stats
import HsBooru.Types hiding (update)
import HsBooru.Util
import HsBooru.Xapian

data GlobalConf = Conf
    { dbDir       :: FilePath
    , optImageDir :: Maybe FilePath
    , batchSize   :: Int
    , parCount    :: Int
    , optCapCount :: Maybe Int
    , retryCount  :: Int
    , minTagCount :: Int
    , blackList   :: [Text]
    , whiteList   :: [Text]
    , verbose     :: Bool
    }

type Command = GlobalConf -> IO ()

siteOpt :: Parser SiteScraper
siteOpt = argument readSite $ metavar "SITE" <> help "booru name"
    where readSite = maybeReader findSite <|> readerError msg
          msg = "Invalid site name. Currently supported sites: "
             ++ unwords (map siteName scrapers)

siteNameOpt :: Parser String
siteNameOpt = siteName <$> siteOpt

runScraper :: [SiteScraper] -> GlobalConf -> InternalDB -> IO ()
runScraper sites Conf{..} acidDB = do
    -- Generate context
    let xapianDir = dbDir </> "xapian"
        imageDir  = Just $ fromMaybe (dbDir </> "images") optImageDir

    forM_ imageDir $ createDirectoryIfMissing True
    xapianDB <- either error id <$> localDB xapianDir

    -- Spawn enough threads to make each capability have about ~parCount open
    -- connections simultaneously
    capCount <- case optCapCount of
        Nothing   -> min 4 <$> getNumProcessors
        Just jobs -> jobs  <$  setNumCapabilities jobs

    let threadCount = capCount * parCount
    fetchURL <- fetchHTTP $ threadCount + 10

    -- Run scraper
    forM_ sites $ \site@SiteScraper{..} -> do
        res <- runBooruM Ctx{..} $ processSite site
        either (logError siteName . show) return res

    A.createArchive acidDB
    log "general" "Done scraping."


-- ** `scrape` command
scrape :: [SiteScraper] -> Command
scrape sites c@Conf{..} = withAcid dbDir $ runScraper sites c

scrapeCmd :: Mod CommandFields Command
scrapeCmd = command "scrape" . info (scrape <$> some siteOpt) $
    progDesc "Scrape posts from websites"

-- ** `update` command
update :: Command
update c@Conf{..} = withAcid dbDir $ \acidDB -> do
    sites <- A.query acidDB ActiveSites
    runScraper [ ss | ss <- scrapers, siteName ss `elem` sites ] c acidDB

updateCmd :: Mod CommandFields Command
updateCmd = command "update" . info (pure update) $
    progDesc "Update all previously scraped websites"

-- ** `retry` command
retrySite :: [String] -> Command
retrySite ss Conf{..} = withAcid dbDir $ \acidDB ->
    forM_ ss $ A.update acidDB . RetrySite

retryCmd :: Mod CommandFields Command
retryCmd = command "retry" . info (retrySite <$> some siteNameOpt) $
    progDesc "Reset the deleted post database for named sites"

-- ** `info` command
siteInfo :: String -> Command
siteInfo site Conf{..} = withAcid dbDir $ \acidDB -> do
    ss <- A.query acidDB (GetSite site)
    putStrLn $ "Stats for site `" ++ site ++ "`:\n"
    printSiteStats ss

infoCmd :: Mod CommandFields Command
infoCmd = command "info" . info (siteInfo <$> siteNameOpt) $
    progDesc "Show some statistics about a named site"

-- * Main

opts :: ParserInfo (IO ())
opts = info (liftA2 ($) parseCmd parseGlobalOpts <**> helper) $ fullDesc
    <> header "hsbooru - a haskell *booru scraper using xapian"
    where parseCmd = hsubparser $ scrapeCmd <> updateCmd <> retryCmd <> infoCmd

main :: IO ()
main = join $ customExecParser parserOpts opts
    where parserOpts = prefs $ showHelpOnError
                            <> showHelpOnEmpty
                            <> multiSuffix ".."
                            <> columns 100

-- ** Global option boilerplate

parseGlobalOpts :: Parser GlobalConf
parseGlobalOpts = Conf
  <$> strOption
    ( long "dbDir"
   <> short 'd'
   <> metavar "DIR"
   <> help "Database directory"
    )

  <*> (optional.strOption)
    ( long "imageDir"
   <> short 'i'
   <> metavar "DIR"
   <> help "Directory to store images in. Defaults to `<dbDir>/images`."
    )

  <*> option auto
    ( long "batchSize"
   <> short 'B'
   <> metavar "N"
   <> showDefault
   <> value 1000
   <> help "How many posts to fetch before committing them all to the \
           \database. Since this is a synchronous operation, using a lower \
           \value reduces throughput; but using a too high value can \
           \create long stalls in the mailbox."
    )

  <*> option auto
    ( long "parallelism"
   <> short 'p'
   <> metavar "N"
   <> showDefault
   <> value 2
   <> help "How many in-flight requests to maintain per thread. Increasing \
           \this can improve throughput but going too high risks running \
           \into network errors as the site kills connections."
    )

  <*> (optional.option auto)
    ( long "jobs"
   <> short 'j'
   <> metavar "N"
   <> help "How many threads to scrape from in parallel. Defaults to the \
           \of detected CPU cores, but no more than 4. Going too high can \
           \be slower, if the server decides to rate limit."
    )

  <*> option auto
    ( long "retryCount"
   <> short 'r'
   <> metavar "N"
   <> showDefault
   <> value 3
   <> help "How often to retry each network request before giving up."
    )

  <*> option auto
    ( long "minTags"
   <> short 'm'
   <> metavar "N"
   <> showDefault
   <> value 0
   <> help "Skip posts with fewer tags than this. They will be retried \
           \automatically"
    )

  <*> (fmap splitTags . many . strOption)
    ( long "blackList"
   <> short 'b'
   <> metavar "TAG"
   <> help "Delete posts with any of these tags."
    )

  <*> (fmap splitTags . many . strOption)
    ( long "whiteList"
   <> short 'w'
   <> metavar "TAG"
   <> help "Delete posts without at least one of these tags."
    )

  <*> switch
    ( long "verbose"
   <> short 'v'
   <> showDefault
   <> help "Print data about every URL and post. Can be slow!"
    )

  where splitTags :: [String] -> [Text]
        splitTags = concatMap $ T.splitOn "," . T.pack
