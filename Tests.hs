{-# LANGUAGE TemplateHaskell, NoMonomorphismRestriction #-}
module Main where

import Test.Framework
import Test.Framework.TH.Prime
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import qualified Data.Acid.Memory.Pure as A
import qualified Data.IntervalSet as IS
import qualified Data.Set as S

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

prop_ss_disjoint ss = IS.null $ presentMap ss `IS.intersection` failedMap ss
prop_ss_union    ss = scrapedMap ss == presentMap ss `IS.union` failedMap ss

prop_ss_superset_pm ss = scrapedMap ss `IS.isSupersetOf` presentMap ss
prop_ss_superset_fm ss = scrapedMap ss `IS.isSupersetOf` failedMap ss

prop_success_overrides i = postSuccess i <> postFailed i == postSuccess i

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

-- mock AcidDB
acidDB = A.openAcidState def
query  = flip A.query
update = flip A.update_

prop_all_sites sites = sites == S.fromList sites'
    where sites' = query ActiveSites $ foldr update acidDB events
          events = [ UpdateSite s def | s <- S.toList sites ]

prop_retry ss = IS.null $ failedMap ss'
    where ss' = query (GetSite "test")
              . update (RetrySite "test")
              $ update (UpdateSite "test" ss) acidDB

-- * HsBooru.Stats

prop_depth ps = avgDepth ps <= fromIntegral (maxDepth ps)
