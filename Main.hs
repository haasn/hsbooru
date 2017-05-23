module Main where

import Prelude hiding (log)

import System.Environment (getArgs)
import qualified Data.Acid as A

import HsBooru.Scraper
import HsBooru.Sites
import HsBooru.Types
import HsBooru.Util
import HsBooru.Xapian

main :: IO ()
main = do
    st   <- acidDB
    db   <- either error id <$> runExceptT xapianDB

    -- To process a site, see if we find a matching scraper, and if so, run it
    let process site = do
            let res = find (\s -> siteName s == site) scrapers
            ss <- requireJust "No scraper found for this site!" $ pure res
            scrapeSite ss db st

    -- When running without a parameter, scrape the previously active
    -- sites. Otherwise, scrape the names sites
    args <- getArgs
    sites <- case args of
                [] -> A.query st ActiveSites
                as -> return as

    forM_ sites $ \site -> do
        res <- runExceptT $ process site
        case res of
            Left e  -> logError site e
            Right _ -> return ()

    -- For cleanup, archive old logs and close the acid DB
    -- Note: the xapian DB is automatically cleaned up after by the GC
    A.createArchive st
    A.closeAcidState st
    log "general" "Done scraping."
