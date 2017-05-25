{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Main where

import Test.Framework
import Test.Framework.TH.Prime
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Acid.Memory.Pure as A
import qualified Data.IntervalSet as IS
import qualified Data.Set as S
import qualified Data.Map as M

import HsBooru.Types
import HsBooru.Stats

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

prop_success_overrides i = successState i <> deletedState i == successState i

prop_subdivide_all p l = map snd (subdivide p l) `sameList` (l :: [Int])

prop_subdivide_union p l = not (null l) ==> IS.unions ps == p
    where ps = map fst $ subdivide p (l :: [()])

prop_subdivide_disjoint p l = and $ zipWith disjoint ps (drop 1 ps)
    where ps = map fst $ subdivide p (l :: [()])
          disjoint x y = IS.null $ IS.intersection x y

instance Arbitrary SiteState where
    -- We have to make sure to preserve the invariant that scrapedMap is a
    -- superset of presentMap
    arbitrary = do
        sm <- arbitrary
        pm <- arbitrary
        return $ SiteState sm (IS.intersection sm pm)

    shrink (SiteState sm pm) =
        [ SiteState sm p | p <- shrink pm ]
     ++ [ SiteState s $ IS.intersection s pm | s <- shrink sm ]

instance Arbitrary ScraperState where
    arbitrary = ScraperState <$> arbitrary
    shrink (ScraperState m) = ScraperState <$> shrink m

-- mock database
acidDB = A.openAcidState
query  = flip A.query
update = flip A.update_

touchSite s = UpdateSites . postState $ PostDeleted { siteID = 1, postSite = s }

prop_all_sites sites = sites == S.fromList sites'
    where sites' = query ActiveSites $ foldr update (acidDB def) events
          events = map touchSite $ S.toList sites

prop_retry ss = all IS.null [ deletedMap $ query (GetSite s) st | s <- sites ]
    where sites = M.keys $ scraperState ss
          st = foldr (update . RetrySite) (acidDB ss) sites

-- * HsBooru.Stats

prop_depth ps = avgDepth ps <= fromIntegral (maxDepth ps)
