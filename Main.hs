module Main where

import Prelude hiding (log)

import Options.Applicative
import Data.Semigroup ((<>))
import System.Environment (getArgs)
import qualified Data.Acid as A

import HsBooru.Conf
import HsBooru.Scraper
import HsBooru.Sites
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

data ScrapeOpts = ScrapeOpts
    { extraSites :: [SiteScraper]
    , update     :: Bool
    , retry      :: Bool
    }

scrapeOpts :: Parser ScrapeOpts
scrapeOpts = ScrapeOpts
    <$> many (argument readSite ( metavar "SITE.."
                               <> help "Sites to scrape" ))
    <*> switch ( long "update"
              <> short 'u'
              <> help "Update all previously scraped sites" )

    <*> switch ( long "retry"
              <> short 'r'
              <> help "Retry all previously failed posts as well as new posts" )

    where readSite = maybeReader $ \sn -> find (\s -> siteName s == sn) scrapers

opts :: ParserInfo ScrapeOpts
opts = info (scrapeOpts <**> helper)
    ( fullDesc
   <> header "hsbooru - a haskell *booru scraper using xapian"
   <> progDesc longDesc )

    where longDesc = "Scrape posts from *booru sites, download the images, "
                  ++ "and store metadata in a xapian DB. Currently supported "
                  ++ "sites:\n"
                  ++ unlines [ siteName | SiteScraper{..} <- scrapers ]

main :: IO ()
main = execParser opts >>= \ScrapeOpts{..} -> withAcid $ \st -> do
    -- Compute the list of sites to scrape
    active <- if update then A.query st ActiveSites else pure []
    let sites = filter (\s -> siteName s `elem` active) scrapers ++ extraSites

    -- Make sure this list is non-empty, otherwise fail with a useful error
    when (null sites) . handleParseResult . Failure $
        parserFailure defaultPrefs opts (ErrorMsg "No sites specified!") []

    db <- either error id <$> runExceptT (xapianDB xapianDir)
    forM_ sites $ \site@SiteScraper{..} -> do
        when retry $ A.update st (RetrySite siteName)
        res <- runExceptT $ scrapeSite site db st
        case res of
            Left e  -> logError siteName e
            Right _ -> return ()

    A.createArchive st
    log "general" "Done scraping."
