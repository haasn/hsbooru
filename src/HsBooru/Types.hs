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
    , module Data.Time
    , module Network.HTTP.Client
    , module System.FilePath
    , HasCallStack

    -- * Main types
    , Context(..)
    , URL
    , fetchHTTP
    , BooruM(BooruM)
    , Err(..)
    , err
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
    , makeSiteState

    -- * IntSets and utilities
    , IntSet
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
    , query
    , update
    ) where

import Control.Applicative
import Control.Exception (try, bracket)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.State (modify)

import Data.SafeCopy
import Data.Default
import Data.IntervalSet (IntSet)
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Time (UTCTime)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import System.FilePath
import GHC.Stack

import qualified Data.Acid as A
import qualified Data.IntervalSet as IS
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as LBS

import HsBooru.Xapian.FFI (XapianDB)

-- * Scraper internal state and database

-- Invariant: successMap and failedMap are disjoint
data SiteState_v0 = SiteState_v0
    { successMap_v0 :: IntSet
    , failedMap_v0  :: IntSet
    }

data SiteState_v1 = SiteState_v1
    { scrapedMap_v1 :: IntSet
    , presentMap_v1 :: IntSet
    }

instance Migrate SiteState_v1 where
    type MigrateFrom SiteState_v1 = SiteState_v0
    migrate SiteState_v0{..} = SiteState_v1{..}
        where scrapedMap_v1 = successMap_v0 `IS.union` failedMap_v0
              presentMap_v1 = successMap_v0

-- | The internal state we store for a site, which is basically just a map of
-- the site's scraped / downloaded post range. Invariant: presentMap is a
-- subset of scrapedMap
data SiteState = SiteState
    { scrapedMap :: IntSet -- ^ Posts that were successfully scraped
    , presentMap :: IntSet -- ^ Posts that were successfully downloaded
    , authorMap  :: IntSet -- ^ Author IDs that we've heard about
    , knownMap   :: IntSet -- ^ Author IDs that we know
    } deriving (Show, Eq)

instance Migrate SiteState where
    type MigrateFrom SiteState = SiteState_v1
    migrate SiteState_v1{..} = SiteState{..}
        where scrapedMap = scrapedMap_v1
              presentMap = presentMap_v1
              authorMap  = IS.empty
              knownMap   = IS.empty

instance Monoid SiteState where
    mempty = def
    mappend (SiteState sa pa aa ka) (SiteState sb pb ab kb) = SiteState{..}
        where scrapedMap = IS.union sa sb
              presentMap = IS.union pa pb
              authorMap  = IS.union aa ab
              knownMap   = IS.union ka kb

instance Default SiteState where
    def = SiteState IS.empty IS.empty IS.empty IS.empty

-- | Arguments: scrapedMap, presentMap, authorMap, knownMap.
-- The superset invariants are enforced
makeSiteState :: IntSet -> IntSet -> IntSet -> IntSet -> SiteState
makeSiteState sm pm am km = SiteState
    { scrapedMap = sm
    , presentMap = IS.intersection sm pm
    , authorMap  = am
    , knownMap   = IS.intersection am km
    }

-- | Posts that were scraped but marked as missing/deleted
deletedMap :: SiteState -> IntSet
deletedMap SiteState{..} = scrapedMap `IS.difference` presentMap

-- | The basic constructors for the site state, representing the state of a
-- single file (either successfully downloaded or marked as deleted)
deletedState :: Int -> SiteState
deletedState id = def { scrapedMap = IS.singleton id }

-- | In contrast to deletedState, this also takes an authorID as a second
-- parameter
successState :: Int -> Int -> SiteState
successState id author =
    def { presentMap = IS.singleton id
        , scrapedMap = IS.singleton id
        , authorMap  = IS.singleton author
        }

-- | Helper function to implement divide-and-conquer algorithms. Given a range
-- of input values, subdivides the int set onto these input values in a manner
-- that tries to be fair. The resulting postsets are guaranteed to be disjoint,
-- although they're not guaranteed to be non-empty.
subdivide :: IntSet -> [a] -> [(IntSet, a)]
subdivide _ [ ] = []
subdivide s [a] = [(s, a)]
subdivide ss as = subdivide sl al ++ subdivide sr ar
    where (sl, sr) = splitIS ss
          [al, ar] = transpose $ chunksOf 2 as

-- Try to fairly split an intervalset in half by splitting along the estimated
-- median
splitIS :: IntSet -> (IntSet, IntSet)
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
deriveSafeCopy 1 'extension ''SiteState_v1
deriveSafeCopy 2 'extension ''SiteState
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
    { xapianDB    :: XapianDB
    , acidDB      :: InternalDB
    , imageDir    :: Maybe FilePath
    , fetchURL    :: URL -> BooruM LBS.ByteString
    , retryCount  :: Int
    , batchSize   :: Int
    , threadCount :: Int
    , minTagCount :: Int
    , blackList   :: [Text]
    , whiteList   :: [Text]
    , verbose     :: Bool
    }

type URL = String

fetchHTTP :: Int -> IO (URL -> BooruM LBS.ByteString)
fetchHTTP n = fetch <$> newManager tlsManagerSettings { managerConnCount = n }

-- | Fetch a URL and return its contents as a ByteString. Retries a number
-- of times according to retryCount
fetch :: Manager -> URL -> BooruM LBS.ByteString
fetch mgr url = ask >>= \Ctx{..} -> retry retryCount $ do
    req <- liftIO $ parseRequest url
    res <- tryHttp $ httpLbs req mgr
    let status = responseStatus res
    unless (statusIsSuccessful status) $
        throwB $ "Error downloading " ++ url ++ ": " ++ show status
    return $ responseBody res
  where tryHttp act = do
            res <- liftIO $ try act
            case res of
                Left e  -> throwB $ show (e :: HttpException)
                Right r -> return r

        retry 1 a = a
        retry n a = a `catchB` (\_ -> retry (n-1) a)


-- | Internal monad for early termination and configuration
newtype BooruM a = BooruM { runBooruM_ :: ExceptT Err (ReaderT Context IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Context)

data Err = Err { errMsg :: String, errLoc :: CallStack }

instance Show Err where
    show Err{..} = unlines [ "Error: " ++ errMsg, prettyCallStack errLoc ]

err :: HasCallStack => String -> Err
err errMsg = Err { errLoc = callStack, .. }

ioEither :: HasCallStack => IO (Either String a) -> BooruM a
ioEither = either throwB pure <=< liftIO

runBooruM :: Context -> BooruM a -> IO (Either Err a)
runBooruM ctx = flip runReaderT ctx . runExceptT . runBooruM_

throwB :: HasCallStack => String -> BooruM a
throwB = BooruM . throwE . err

catchB :: BooruM a -> (Err -> BooruM a) -> BooruM a
catchB a h = BooruM $ runBooruM_ a `catchE` (runBooruM_ . h)

lower :: BooruM a -> BooruM (IO (Either Err a))
lower (BooruM et) = runReaderT (runExceptT et) <$> ask

-- | A scraped post, including all metadata and status
data Post
    -- | Post was successfully scraped
    = PostSuccess { siteID   :: Int
                  , uploaded :: UTCTime
                  , postSite :: String
                  , rating   :: Rating
                  , uploader :: Int
                  , score    :: Int
                  , source   :: (Maybe Text)
                  , fileURL  :: Text
                  , fileName :: Text
                  , tags     :: [Text] }
    -- | Post scrape was attempted and failed for some reason, included
    | PostFailure { siteID   :: Int
                  , postSite :: String
                  , reason   :: String }
    -- | Post scrape was successful but the file was confirmed to be deleted
    | PostDeleted { siteID   :: Int
                  , postSite :: String }
    deriving (Eq, Show)

data Rating = Safe | Questionable | Explicit
    deriving (Eq, Show, Read)

postState :: Post -> ScraperState
postState PostFailure{..} = def
postState post = ScraperState . M.singleton (postSite post) $ case post of
    PostDeleted{..} -> deletedState siteID
    PostSuccess{..} -> successState siteID uploader
    _ -> def

-- | A site-specific scraper definition
data SiteScraper = SiteScraper
    { siteName :: String
    , idRange  :: BooruM IntSet
    , scrapeID :: Int -> BooruM Post
    }

-- Utility

type InternalDB = A.AcidState ScraperState

-- | Opens the acid DB given the dbDir path
withAcid :: FilePath -> (InternalDB -> IO a) -> IO a
withAcid dbDir = bracket (A.openLocalStateFrom acidDir def) A.closeAcidState
    where acidDir = dbDir </> "acid"

query :: A.EventState ev ~ ScraperState
      => A.QueryEvent ev => ev -> BooruM (A.EventResult ev)
query q = asks acidDB >>= \st ->
    liftIO $ A.query st q

update :: A.EventState ev ~ ScraperState
       => A.UpdateEvent ev => ev -> BooruM (A.EventResult ev)
update u = asks acidDB >>= \st ->
    liftIO $ A.update st u
