{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction, OverloadedStrings #-}
module Main where

import Test.Framework
import Test.Framework.TH.Prime
import Test.Framework.Providers.QuickCheck2
import Test.Framework.Providers.HUnit
import Test.QuickCheck hiding (verbose)
import Test.HUnit

import Data.Char
import Data.Time

import qualified Data.Acid as A
import qualified Data.Acid.Memory as A
import qualified Data.ByteString.Lazy as LBS
import qualified Data.IntervalSet as IS
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as Set
import qualified Streaming.Prelude as S

import HsBooru.Types
import HsBooru.Scraper
import HsBooru.Sites
import HsBooru.Stats
import HsBooru.Util
import HsBooru.Xapian

main = $(defaultMainGenerator)

-- * Utilities

sameList :: Ord a => [a] -> [a] -> Bool
sameList x y = sort x == sort y

-- * HsBooru.Types

instance Arbitrary IS.IntSet where
    arbitrary = IS.fromList <$> arbitrary
    shrink = map IS.fromList . shrink . IS.toList

prop_ss_disjoint ss = IS.null $ presentMap ss `IS.intersection` deletedMap ss
prop_ss_union    ss = scrapedMap ss == presentMap ss `IS.union` deletedMap ss

prop_ss_superset_pm ss = scrapedMap ss `IS.isSupersetOf` presentMap ss
prop_ss_superset_fm ss = scrapedMap ss `IS.isSupersetOf` deletedMap ss

prop_success_overrides i = successState i 0 <> deletedState i == successState i 0

prop_subdivide_all p l = map snd (subdivide p l) `sameList` (l :: [Int])

prop_subdivide_union p l = not (null l) ==> IS.unions ps == p
    where ps = map fst $ subdivide p (l :: [()])

prop_subdivide_disjoint p l = and $ zipWith disjoint ps (drop 1 ps)
    where ps = map fst $ subdivide p (l :: [()])
          disjoint x y = IS.null $ IS.intersection x y

instance Arbitrary SiteState where
    arbitrary = makeSiteState <$> arb <*> arb <*> arb <*> arb
        where arb = arbitrary

    shrink (SiteState sm pm am km) =
         [ makeSiteState s' pm am km | s' <- shrink sm ]
      ++ [ makeSiteState sm p' am km | p' <- shrink pm ]
      ++ [ makeSiteState sm pm a' km | a' <- shrink am ]
      ++ [ makeSiteState sm pm am k' | k' <- shrink km ]

instance Arbitrary ScraperState where
    arbitrary = ScraperState <$> arbitrary
    shrink (ScraperState m) = ScraperState <$> shrink m

-- ** Mock context

type Server = [(URL, LBS.ByteString)]

testContext :: ScraperState -> Server -> IO Context
testContext init srv = do
    xpDB <- either error id <$> memoryDB
    stDB <- A.openMemoryState init
    return Ctx { xapianDB    = xpDB
               , acidDB      = stDB
               , imageDir    = Nothing
               , fetchURL    = fakeFetch srv
               , retryCount  = 1
               , batchSize   = 1
               , threadCount = 1
               , minTagCount = 0
               , verbose     = False
               }

fakeFetch :: Server -> URL -> BooruM LBS.ByteString
fakeFetch srv url = case find (\(u, _) -> u == url) srv of
    Just (_, res) -> return res
    Nothing       -> throwB "URL not found in testContext!"

runBooruWith :: ScraperState -> Server -> BooruM a -> IO a
runBooruWith init srv act = do
    ctx <- testContext init srv
    either error id <$> runBooruM ctx act

-- ** Some mock database tests

touchSite :: String -> BooruM ()
touchSite s = update $ UpdateSites ss
    where ss = postState $ PostDeleted { siteID = 1, postSite = s }

prop_all_sites sites = ioProperty . runBooruWith def [] $ do
    mapM_ touchSite $ Set.toList sites
    sites' <- Set.fromList <$> query ActiveSites
    return $ sites == sites'

prop_retry ss = ioProperty . runBooruWith ss [] $ do
    let sites = M.keys $ scraperState ss
    mapM_ (update . RetrySite) sites
    res <- sequence [ query (GetSite s) | s <- sites ]
    return $ all (IS.null . deletedMap) res

-- * HsBooru.Scraper

-- ** gelbooru scraper

gelbooruSrv :: Server
gelbooruSrv = [
    ( "https://gelbooru.com/index.php?page=dapi&s=post&q=index&id=1"
    , "<?xml version=\"1.0\" encoding=\"UTF-8\"?><posts count=\"1\" offset=\"0\"><post height=\"600\" score=\"266\" file_url=\"//simg4.gelbooru.com/images/f3/82/f3824ad985f121187065c4eaeae22875.jpg\" parent_id=\"\" sample_url=\"//simg4.gelbooru.com/images/f3/82/f3824ad985f121187065c4eaeae22875.jpg\"  sample_width=\"400\" sample_height=\"600\" preview_url=\"//assets.gelbooru.com/thumbnails/f3/82/thumbnail_f3824ad985f121187065c4eaeae22875.jpg\" rating=\"s\" tags=\"1girl asahina_mikuru asahina_mikuru_(cosplay) breasts brown_hair cleavage corset cosplay dress female from_above get hairband lips long_hair looking_at_viewer lowres mikuru_beam mizuhara_arisa name_tag pantyhose photo sitting smile solo suzumiya_haruhi_no_yuuutsu v waitress wrist_cuffs\" id=\"1\" width=\"400\"  change=\"1495758432\" md5=\"f3824ad985f121187065c4eaeae22875\" creator_id=\"6498\" has_children=\"true\" created_at=\"Mon Jul 16 00:19:58 -0500 2007\" status=\"active\" source=\"\" has_notes=\"false\" has_comments=\"true\" preview_width=\"100\" preview_height=\"150\"/>"
    ),

    -- Invalid creator_id / created_at
    ( "https://gelbooru.com/index.php?page=dapi&s=post&q=index&id=198602"
    , "<?xml version=\"1.0\" encoding=\"UTF-8\"?><posts count=\"1\" offset=\"0\"><post score=\"1\" file_url=\"//assets.gelbooru.com/images/71/62/7162356ed0764b24f1318488a7e324ce.jpg\" rating=\"s\" tags=\" snip \" id=\"198602\" creator_id=\"\" created_at=\"Wed Dec 31 18:00:00 -0600 1969\" source=\"\" />"
    )
  ]


case_gelbooru_real :: Assertion
case_gelbooru_real = do
    ctx <- testContext def gelbooruSrv
    Right [p] <- runBooruM ctx . S.toList_ $ scrapeSite gelbooru (IS.singleton 1)
    p @?= PostSuccess
        { siteID   = 1
        , uploaded = UTCTime (fromGregorian 2007 07 16) (secondsToDiffTime 19198)
        , postSite = "gelbooru"
        , rating   = Safe
        , uploader = 6498
        , score    = 266
        , source   = Nothing
        , fileURL  = "http://simg4.gelbooru.com/images/f3/82/f3824ad985f121187065c4eaeae22875.jpg"
        , fileName = "f3824ad985f121187065c4eaeae22875.jpg"
        , tags     = ["1girl","asahina_mikuru","asahina_mikuru_(cosplay)","breasts","brown_hair","cleavage","corset","cosplay","dress","female","from_above","get","hairband","lips","long_hair","looking_at_viewer","lowres","mikuru_beam","mizuhara_arisa","name_tag","pantyhose","photo","sitting","smile","solo","suzumiya_haruhi_no_yuuutsu","v","waitress","wrist_cuffs"]
        }

case_gelbooru_invalid :: Assertion
case_gelbooru_invalid = do
    ctx <- testContext def gelbooruSrv
    Right [p] <- runBooruM ctx . S.toList_ $ scrapeSite gelbooru (IS.singleton 198602)
    p @?= PostSuccess
        { siteID   = 198602
        , uploaded = UTCTime (fromGregorian 1970 01 01) (secondsToDiffTime 0)
        , postSite = "gelbooru"
        , rating   = Safe
        , uploader = 0
        , score    = 1
        , source   = Nothing
        , tags     = [ "snip" ]
        , fileURL  = "http://assets.gelbooru.com/images/71/62/7162356ed0764b24f1318488a7e324ce.jpg"
        , fileName = "7162356ed0764b24f1318488a7e324ce.jpg"
        }

-- * HsBooru.Stats

prop_depth ps = avgDepth ps <= fromIntegral (maxDepth ps)

-- * HsBooru.Xapian

instance Arbitrary Text where
    arbitrary = T.pack <$> arbitrary
    shrink    = map T.pack . shrink . T.unpack

prop_sanitize_valid = T.all isValid . sanitizeTag
    where isValid '_' = True
          isValid  c  = isAlphaNum c

prop_sanitize_nonempty = not . T.null . sanitizeTag

case_sanitize_easy   = sanitizeTag "one-piece_swimsuit" @?= "one_piece_swimsuit"
case_sanitize_tricky = sanitizeTag ":)" @?= "_"
case_sanitize_empty  = sanitizeTag ""   @?= "_"
