{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (log)

import Data.Monoid ((<>))
import Options.Applicative
import System.Environment (getArgs)

import qualified Data.Acid as A

import HsBooru.Conf
import HsBooru.Scraper
import HsBooru.Sites
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

type Command = Mod CommandFields (InternalDB -> IO ())

siteOpt :: Parser SiteScraper
siteOpt = argument readSite $ metavar "SITE" <> help "booru name"
    where readSite = maybeReader findSite <|> readerError err
          err = "Invalid site name. Currently supported sites: "
             ++ unwords (map siteName scrapers)

siteNameOpt :: Parser String
siteNameOpt = siteName <$> siteOpt

-- `scrape` command
scrape :: [SiteScraper] -> InternalDB -> IO ()
scrape sites st = do
    db <- either error id <$> runExceptT (xapianDB xapianDir)
    forM_ sites $ \site@SiteScraper{..} -> do
        res <- runExceptT . retry retryCount $ scrapeSite site db st
        case res of
            Left e  -> logError siteName e
            Right _ -> return ()

    A.createArchive st
    log "general" "Done scraping."

scrapeCmd :: Command
scrapeCmd = command "scrape" . info (scrape <$> some siteOpt) $
    progDesc "Scrape posts from websites"

-- `update` command
update :: InternalDB -> IO ()
update st = do
    sites <- A.query st ActiveSites
    scrape [ ss | ss <- scrapers, siteName ss `elem` sites ] st

updateCmd :: Command
updateCmd = command "update" . info (pure update) $
    progDesc "Update all previously scraped websites"

-- `retry` command
retrySite :: [String] -> InternalDB -> IO ()
retrySite ss st = forM_ ss $ A.update st . RetrySite

retryCmd :: Command
retryCmd = command "retry" . info (retrySite <$> some siteNameOpt) $
    progDesc "Reset the failed post database for named sites"

-- Main

opts :: ParserInfo (InternalDB -> IO ())
opts = info (hsubparser commands <**> helper) $
    fullDesc
 <> header "hsbooru - a haskell *booru scraper using xapian"

    where commands = scrapeCmd <> updateCmd <> retryCmd

main :: IO ()
main = customExecParser parserOpts opts >>= withAcid
    where parserOpts = prefs $ showHelpOnError
                            <> showHelpOnEmpty
                            <> multiSuffix ".."
