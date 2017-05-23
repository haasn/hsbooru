{-# LANGUAGE CPP, BangPatterns #-}
module HsBooru.Stats (printStats) where

#include "MachDeps.h"

import Text.Printf

import Data.IntervalSet (IntSet(..))
import qualified Data.IntervalSet as IS

import HsBooru.Types

-- | Print out a summary of site-related statistics
printStats :: SiteState -> IO ()
printStats site@SiteState{..} = do
    let [total, okay, fail] = map IS.size [scrapedMap, presentMap, failedMap]
        failedMap = scrapedMap `IS.difference` presentMap

    printf "Scraped count: %d\n" total
    printf "Success count: %d\n" okay
    printf "Failed count:  %d\n" fail
    printf "=> Success ratio: %.2f%%\n" $ 100 * okay // total
    printf "=> Deleted ratio: %.2f%%\n" $ 100 * fail // total
    printf "\n"

    printf "## scrapedMap stats:\n"
    isStats scrapedMap
    printf "\n"

    printf "## presentMap stats:\n"
    isStats presentMap

(//) :: (Integral a, Integral b) => a -> b -> Double
a // b = fromIntegral a / fromIntegral b


-- Shamelessly inspired by Data.IntervalSet.Internal
binCount :: IntSet -> Int
binCount (Bin _ _ l r) = 1 + binCount l + binCount r
binCount _ = 0

leafCount :: IntSet -> Int
leafCount (Bin _ _ l r) = leafCount l + leafCount r
leafCount _ = 1

maxDepth :: IntSet -> Int
maxDepth (Bin _ _ l r) = 1 + max (maxDepth l) (maxDepth r)
maxDepth _ = 0

avgDepth :: IntSet -> Double
avgDepth s = let (st, ct) = go 0 s in st / ct
    where go d (Bin _ _ l r) = let (!sl, !cl) = go (d+1) l
                                   (!sr, !cr) = go (d+1) r
                               in (sl+sr, cl+cr)
          go d _ = (d, 1)

wordCount :: IntSet -> Int
wordCount (Bin _ _ l r) = 5 + wordCount l + wordCount r
wordCount (Tip _ _)     = 3
wordCount (Fin _ _)     = 3
wordCount  Nil          = 1

origSize :: IntSet -> Int
origSize (Bin _ _ l r) = 5 + origSize l + origSize r
origSize (Tip _ _)     = 3
origSize (Fin _ m)     =
  let tips = m `div` WORD_SIZE_IN_BITS
      bins = tips - 1
  in tips * 3 + bins * 5
origSize  Nil          = 1

word_size_in_bytes :: Int
word_size_in_bytes = WORD_SIZE_IN_BITS `div` 8

isStats :: IntSet -> IO ()
isStats s = do
  let actualSize = fromIntegral $ leafCount s
      idealSize  = 2 ** avgDepth s
      idealDepth = logBase 2 actualSize

  printf "Bin count:             %d\n"     $ binCount s
  printf "Leaf count:            %d\n"     $ leafCount s
  printf "Maximum depth:         %d\n"     $ maxDepth s
  printf "Average depth:         %.2f\n"   $ avgDepth s
  printf "Ideal depth:           %.2f\n"   $ idealDepth
  printf "Balancedness:          %.2f%%\n" $ 100 * idealDepth / avgDepth s

  let treeSize = word_size_in_bytes * wordCount s
      normSize = word_size_in_bytes * origSize s
      bsSize | IS.null s = 24
             | otherwise = 24 + IS.findMax s `div` 8

      savedSpace   = (normSize - treeSize) // normSize
      savedSpaceBS = (bsSize   - treeSize) // bsSize

  printf "Size in bytes:         %d\n"     $ treeSize
  printf "Saved over dense set:  %.2f%%\n" $ 100 * savedSpace
  printf "Saved over bytestring: %.2f%%\n" $ 100 * savedSpaceBS
