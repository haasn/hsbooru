{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)

import Data.Monoid ((<>))
import Options.Applicative

import GHC.Conc (getNumProcessors)
import Control.Concurrent
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.FilePath

import qualified Data.Acid as A

import HsBooru.Scraper
import HsBooru.Sites
import HsBooru.Stats
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

data GlobalConf = Conf
    { dbDir       :: FilePath
    , optImageDir :: Maybe FilePath
    , batchSize   :: Int
    , parCount    :: Int
    , optCapCount :: Maybe Int
    , retryCount  :: Int
    }

type Command = GlobalConf -> IO ()

siteOpt :: Parser SiteScraper
siteOpt = argument readSite $ metavar "SITE" <> help "booru name"
    where readSite = maybeReader findSite <|> readerError err
          err = "Invalid site name. Currently supported sites: "
             ++ unwords (map siteName scrapers)

siteNameOpt :: Parser String
siteNameOpt = siteName <$> siteOpt

-- ** `scrape` command
scrape :: [SiteScraper] -> Command
scrape sites Conf{..} = withAcid dbDir $ \acidDB -> do
    -- Generate context
    let xapianDir = dbDir </> "xapian"
        imageDir  = fromMaybe (dbDir </> "images") optImageDir

    createDirectoryIfMissing True imageDir
    xapianDB <- either error id <$> localDB xapianDir

    -- Spawn enough threads to make each capability have about ~parCount open
    -- connections simultaneously
    capCount <- case optCapCount of
        Nothing   -> min 4 <$> getNumProcessors
        Just jobs -> jobs  <$  setNumCapabilities jobs

    let threadCount = capCount * parCount
    manager  <- spawnManager $ threadCount + 10

    -- Run scraper
    forM_ sites $ \site@SiteScraper{..} -> do
        res <- runBooruM Ctx{..} $ processSite site
        case res of
            Left e  -> logError siteName e
            Right _ -> return ()

    A.createArchive acidDB
    log "general" "Done scraping."

scrapeCmd :: Mod CommandFields Command
scrapeCmd = command "scrape" . info (scrape <$> some siteOpt) $
    progDesc "Scrape posts from websites"

-- ** `update` command
update :: Command
update c@Conf{..} = withAcid dbDir $ \acidDB -> do
    sites <- A.query acidDB ActiveSites
    scrape [ ss | ss <- scrapers, siteName ss `elem` sites ] c

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
    printStats ss

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
   <> help ("Directory to store images in. Defaults to `<dbDir>/images`.")
    )

  <*> option auto
    ( long "batchsize"
   <> short 'b'
   <> metavar "N"
   <> showDefault
   <> value 1000
   <> help ("How many posts to fetch before committing them all to the "++
            "database. Since this is a synchronous operation, using a lower "++
            "value reduces throughput.")
    )

  <*> option auto
    ( long "parallelism"
   <> short 'p'
   <> metavar "N"
   <> showDefault
   <> value 2
   <> help ("How many in-flight requests to maintain per thread. Increasing "++
            "this can improve throughput but going too high risks running "++
            "into network errors as the site kills connections.")
    )

  <*> (optional.option auto)
    ( long "jobs"
   <> short 'j'
   <> metavar "N"
   <> help ("How many threads to scrape from in parallel. Defaults to the "++
            "of detected CPU cores, but no more than 4. Going too high can "++
            "be slower, if the server decides to rate limit.")
    )

  <*> option auto
    ( long "retryCount"
   <> short 'r'
   <> metavar "N"
   <> showDefault
   <> value 3
   <> help ("How often to retry each network request before giving up.")
    )
