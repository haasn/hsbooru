{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances #-}
-- | Convenience module for types and instances, also re-exports helpful common
-- imports
module HsBooru.Types (
    -- * Module re-exports for convenience
      module Control.Monad
    , module Control.Monad.Trans.Except
    , module Data.Default
    , module Data.List
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Text
    , module Network.HTTP.Client

    -- * Main types
    , Post(..)
    , Rating(..)
    , SiteScraper(..)
    , SiteState(..)
    , InternalDB

    -- * PostSets and utilities
    , PostSet
    , postSuccess
    , postFailed
    , subdivide

    -- * AcidState helpers/queries
    , ActiveSites(..)
    , GetSite(..)
    , UpdateSite(..)
    , RetrySite(..)
    , withAcid
    ) where

import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Reader (asks)
import Control.Monad.State (modify)

import Data.SafeCopy
import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Client

import qualified Data.Acid as A
import qualified Data.IntervalSet as IS
import qualified Data.Map as M

import HsBooru.Conf

-- | A scraped post, including all metadata
data Post = Post
    { siteID   :: !Int
    , booru    :: !String
    , rating   :: !Rating
    , uploader :: !Int
    , score    :: !Int
    , source   :: !(Maybe Text)
    , fileURL  :: !Text
    , fileName :: !Text
    , tags     :: ![Text]
    } deriving Show

data Rating = Safe | Questionable | Explicit
    deriving (Show, Read)

-- | A site-specific scraper definition
data SiteScraper = SiteScraper
    { siteName :: String
    , idRange  :: Manager -> ExceptT String IO PostSet
    , scrapeID :: Manager -> Int -> ExceptT String IO (Maybe Post)
    }

-- Scraper internal state and database

-- | An efficient (interval-packed) set of post IDs
type PostSet = IS.IntSet

-- Invariant: successMap and failedMap are disjoint
data SiteState_v0 = SiteState_v0
    { successMap_v0 :: !PostSet
    , failedMap_v0  :: !PostSet
    }

-- | The internal state we store for a site, which is basically just a map of
-- the site's scraped / downloaded post range. Invariant: presentMap is a
-- subset of scrapedMap
data SiteState = SiteState
    { scrapedMap :: !PostSet
    , presentMap :: !PostSet
    }

instance Migrate SiteState where
    type MigrateFrom SiteState = SiteState_v0
    migrate SiteState_v0{..} = SiteState{..}
        where scrapedMap = successMap_v0 `IS.union` failedMap_v0
              presentMap = successMap_v0

instance Monoid SiteState where
    mempty = def
    mappend (SiteState sa pa) (SiteState sb pb) = SiteState{..}
        where scrapedMap = IS.union sa sb
              presentMap = IS.union pa pb

instance Default SiteState where
    def = SiteState IS.empty IS.empty

-- | The basic constructors for the site state, representing the state of a
-- single file (either successfully downloaded or marked as
-- failed/missing/deleted).
postSuccess, postFailed :: Int -> SiteState
postSuccess id = (postFailed id) { presentMap = IS.singleton id }
postFailed  id = def { scrapedMap = IS.singleton id }

-- | Helper function to implement divide-and-conquer algorithms. Given a range
-- of input values, subdivides the post set onto these input values in a manner
-- that tries to be fair. The resulting postsets are guaranteed to be disjoint,
-- although they're not guaranteed to be non-empty.
subdivide :: PostSet -> [a] -> [(PostSet, a)]
subdivide _ [ ] = []
subdivide s [a] = [(s, a)]
subdivide ss as = subdivide (IS.insert mid sl) al ++ subdivide sr ar
    where -- split the post set by estimating the center
          (sl, sr) = IS.split mid ss
          mid = (IS.findMin ss + IS.findMax ss) `div` 2
          -- split the value set by taking every other element
          [al, ar] = transpose $ chunksOf 2 as

-- Can't be a newtype due to overly strict nominal role constraints on SafeCopy
type ScraperState = M.Map String SiteState

-- acid-state integration

deriveSafeCopy 0 'base ''IS.IntSet
deriveSafeCopy 0 'base ''SiteState_v0
deriveSafeCopy 1 'extension ''SiteState

activeSites :: A.Query ScraperState [String]
activeSites = asks M.keys

getSite :: String -> A.Query ScraperState SiteState
getSite k = asks $ fromMaybe def . M.lookup k

updateSite :: String -> SiteState -> A.Update ScraperState ()
updateSite k = modify . M.insertWith mappend k

retrySite :: String -> A.Update ScraperState ()
retrySite = modify . M.adjust reset
    where reset ss = ss { scrapedMap = presentMap ss }

A.makeAcidic ''ScraperState ['activeSites, 'getSite, 'updateSite, 'retrySite]

-- Utility

type InternalDB = A.AcidState ScraperState

withAcid :: (InternalDB -> IO a) -> IO a
withAcid = bracket (A.openLocalStateFrom acidDir M.empty) A.closeAcidState
