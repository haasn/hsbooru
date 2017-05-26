{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances,
             GeneralizedNewtypeDeriving #-}
-- | Convenience module for types and instances, also re-exports helpful common
-- imports
module HsBooru.Types (
    -- * Module re-exports for convenience
      module Control.Applicative
    , module Control.Monad
    , module Control.Monad.Reader
    , module Data.Default
    , module Data.List
    , module Data.Maybe
    , module Data.Monoid
    , module Data.Text
    , module Network.HTTP.Client
    , module System.FilePath

    -- * Main types
    , Context(..)
    , spawnManager
    , BooruM(BooruM)
    , runBooruM
    , ioEither
    , throwB
    , catchB
    , lower

    , Post(..)
    , Rating(..)
    , SiteScraper(..)
    , SiteState(..)
    , ScraperState(..)
    , InternalDB

    -- * PostSets and utilities
    , PostSet
    , successState
    , deletedState
    , postState
    , postCount
    , subdivide
    , deletedMap

    -- * AcidState helpers/queries
    , ActiveSites(..)
    , GetSite(..)
    , UpdateSites(..)
    , RetrySite(..)
    , withAcid
    ) where

import Control.Applicative
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.State (modify)

import Data.SafeCopy
import Data.Default
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.FilePath

import qualified Data.Acid as A
import qualified Data.IntervalSet as IS
import qualified Data.Map as M

import HsBooru.Xapian.FFI (XapianDB)

-- * Scraper internal state and database

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
    { scrapedMap :: !PostSet -- ^ Posts that were successfully scraped
    , presentMap :: !PostSet -- ^ Posts that were successfully downloaded
    } deriving (Show, Eq)

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

-- | Posts that were scraped but marked as missing/deleted
deletedMap :: SiteState -> PostSet
deletedMap SiteState{..} = scrapedMap `IS.difference` presentMap

-- | The basic constructors for the site state, representing the state of a
-- single file (either successfully downloaded or marked as deleted)
successState, deletedState :: Int -> SiteState
successState id = (deletedState id) { presentMap = IS.singleton id }
deletedState id = def { scrapedMap = IS.singleton id }

-- | Helper function to implement divide-and-conquer algorithms. Given a range
-- of input values, subdivides the post set onto these input values in a manner
-- that tries to be fair. The resulting postsets are guaranteed to be disjoint,
-- although they're not guaranteed to be non-empty.
subdivide :: PostSet -> [a] -> [(PostSet, a)]
subdivide _ [ ] = []
subdivide s [a] = [(s, a)]
subdivide ss as = subdivide sl al ++ subdivide sr ar
    where (sl, sr) = splitIS ss
          [al, ar] = transpose $ chunksOf 2 as

-- Try to fairly split an intervalset in half by splitting along the estimated
-- median
splitIS :: PostSet -> (PostSet, PostSet)
splitIS ps | IS.null ps = (ps, ps)
splitIS ps = (fix left, right)
    where (left, right) = IS.split mid ps
          mid = (IS.findMin ps + IS.findMax ps) `div` 2
          fix | mid `IS.member` ps = IS.insert mid
              | otherwise = id

-- Can't be a newtype due to overly strict nominal role constraints on SafeCopy
newtype ScraperState = ScraperState { scraperState :: M.Map String SiteState }
    deriving Show

instance Monoid ScraperState where
    mempty = ScraperState mempty
    mappend (ScraperState a) (ScraperState b) = ScraperState $ M.unionWith mappend a b

instance Default ScraperState where
    def = ScraperState def

instance Migrate ScraperState where
    type MigrateFrom ScraperState = M.Map String SiteState
    migrate = ScraperState

postCount :: ScraperState -> Int
postCount = M.foldr (\a n -> siteSize a + n) 0 . scraperState
    where siteSize = IS.size . scrapedMap

-- ** acid-state integration

deriveSafeCopy 0 'base ''IS.IntSet
deriveSafeCopy 0 'base ''SiteState_v0
deriveSafeCopy 1 'extension ''SiteState
deriveSafeCopy 1 'extension ''ScraperState

activeSites :: A.Query ScraperState [String]
activeSites = asks $ M.keys . scraperState

getSite :: String -> A.Query ScraperState SiteState
getSite k = asks $ fromMaybe def . M.lookup k . scraperState

updateSite :: String -> SiteState -> A.Update ScraperState ()
updateSite site = modify . mappend . ScraperState . M.singleton site
{-# DEPRECATED updateSite "Only kept for acid compatibility" #-}

updateSites :: ScraperState -> A.Update ScraperState ()
updateSites ss = modify $ mappend ss

retrySite :: String -> A.Update ScraperState ()
retrySite site = modify $ ScraperState . M.adjust reset site . scraperState
    where reset ss = ss { scrapedMap = presentMap ss }

A.makeAcidic ''ScraperState ['activeSites, 'getSite, 'updateSite, 'updateSites, 'retrySite]

-- | Global context record
data Context = Ctx
    { xapianDB    :: !XapianDB
    , acidDB      :: !InternalDB
    , imageDir    :: !FilePath
    , manager     :: !Manager
    , retryCount  :: !Int
    , batchSize   :: !Int
    , threadCount :: !Int
    , minTagCount :: !Int
    }

spawnManager :: Int -> IO Manager
spawnManager n = newManager $ tlsManagerSettings { managerConnCount = n }

-- | Internal monad for early termination and configuration
newtype BooruM a = BooruM { runBooruM_ :: ExceptT String (ReaderT Context IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

ioEither :: IO (Either String a) -> BooruM a
ioEither = BooruM . ExceptT . lift

runBooruM :: Context -> BooruM a -> IO (Either String a)
runBooruM ctx = flip runReaderT ctx . runExceptT . runBooruM_

throwB :: String -> BooruM a
throwB = BooruM . throwE

catchB :: BooruM a -> (String -> BooruM a) -> BooruM a
catchB a h = BooruM $ runBooruM_ a `catchE` (runBooruM_ . h)

lower :: BooruM a -> BooruM (IO (Either String a))
lower (BooruM et) = runReaderT (runExceptT et) <$> ask

-- | A scraped post, including all metadata and status
data Post
    -- | Post was successfully scraped
    = PostSuccess { siteID   :: !Int
                  , postSite :: !String
                  , rating   :: !Rating
                  , uploader :: !Int
                  , score    :: !Int
                  , source   :: !(Maybe Text)
                  , fileURL  :: !Text
                  , fileName :: !Text
                  , tags     :: ![Text] }
    -- | Post scrape was attempted and failed for some reason, included
    | PostFailure { siteID   :: !Int
                  , postSite :: !String
                  , reason   :: !String }
    -- | Post scrape was successful but the file was confirmed to be deleted
    | PostDeleted { siteID   :: !Int
                  , postSite :: !String }
    deriving Show

data Rating = Safe | Questionable | Explicit
    deriving (Show, Read)

postState :: Post -> ScraperState
postState PostFailure{..} = def
postState PostDeleted{..} = ScraperState $ M.singleton postSite (deletedState siteID)
postState PostSuccess{..} = ScraperState $ M.singleton postSite (successState siteID)

-- | A site-specific scraper definition
data SiteScraper = SiteScraper
    { siteName :: String
    , idRange  :: BooruM PostSet
    , scrapeID :: Int -> BooruM Post
    }

-- Utility

type InternalDB = A.AcidState ScraperState

-- | Opens the acid DB given the dbDir path
withAcid :: FilePath -> (InternalDB -> IO a) -> IO a
withAcid dbDir = bracket (A.openLocalStateFrom acidDir def) A.closeAcidState
    where acidDir = dbDir </> "acid"
