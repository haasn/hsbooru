{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}
module HsBooru.Conf where

import System.FilePath.Posix ((</>))
import Network.HTTP.Client (ManagerSettings(..))
import Network.HTTP.Client.TLS (tlsManagerSettings)

-- * Global configuration and constants

-- Directories
dbDir       = "/booru/"
imageDir    = dbDir </> "images"
xapianDir   = dbDir </> "xapian"
acidDir     = dbDir </> "acid"

-- Some misc tuning options
mgrOpts     = tlsManagerSettings { managerConnCount = max 10 threadCount
                                 , managerRetryableException = const True }

retryCount  = 3 -- How often to retry each request before giving up

-- How many posts to fetch before committing them all to the database.
-- Increasing this can improve throughput and reduce database overhead at the
-- cost of losing more work when you interrupt the program prematuraly.
batchSize   = 200

-- How many scraper threads to run in parallel. Increasing this improves
-- performance, but if you take it too far you will probably get rate limited
-- by the server, at least gelbooru does this. So keep it safe. Incidentally,
-- this still works if the program is running on a single core only, it will
-- just keep open four connections to the server concurrently instead of
-- processing each request in lockstep.
threadCount = 4

-- Term prefix mapping, for reusable tags
booruPrefix    = "B" -- Booru name the post was scraped from
siteIDPrefix   = "I" -- The site-specific ID
uploaderPrefix = "U" -- Post uploader, ID or name (if available)
ratingPrefix   = "R" -- File rating, e.g. `safe` or `questionable`
extPrefix      = "E" -- File extension, e.g. `png`
filePrefix     = "F" -- Filename, so you can search for posts by name
tagPrefix      = ""  -- Generic content tags

-- Value mapping, for per-document unique identification and sorting
siteIDSlot   = 0 -- Site-specific ID
scoreSlot    = 1 -- User score assigned to the website
fileNameSlot = 2 -- Name the file is stored under
fileURLSlot  = 3 -- URL the file was downloaded from
sourceSlot   = 4 -- Website's literal "Source" field, if it has one
