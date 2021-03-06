{-# LANGUAGE CPP, BangPatterns #-}
module HsBooru.Stats
    ( -- * Post statistics
      PostStats(..)
    , showPostStats
    , postStats
    -- * Site statistics
    , printSiteStats
    , avgDepth
    , maxDepth
    -- * Timers and misc utilities
    , Timer
    , newTimer
    , measureTimer
    , showPerf
    ) where

#include "MachDeps.h"

import Text.Printf
import Data.IORef
import System.Clock

import Data.IntervalSet (IntSet(..))
import qualified Data.IntervalSet as IS

import HsBooru.Types

data PostStats = PostStats { good :: Int, gone :: Int, fail :: Int }

instance Semigroup PostStats where
    PostStats a b c <> PostStats x y z = PostStats (a+x) (b+y) (c+z)

instance Monoid PostStats where
    mempty = PostStats 0 0 0

postStats :: Post -> PostStats
postStats PostSuccess{} = PostStats 1 0 0
postStats PostDeleted{} = PostStats 0 1 0
postStats PostFailure{} = PostStats 0 0 1

-- | Helper to pretty-print post count stats
showPostStats :: PostStats -> String
showPostStats PostStats{..} =  "Saved: "   ++ show good
                       ++ " Deleted: " ++ show gone
                       ++ " Failed: "  ++ show fail


-- | Pretty-print performance characteristics, given a count and a time delta
-- in nanoseconds
showPerf :: Int -> Integer -> String
showPerf n dt = printf "%.3f posts/second" $ 10^9 * n // dt

-- Nanoseconds
type Timer = IORef Integer

getClock :: IO Integer
getClock = fmap (\TimeSpec{..} -> fi nsec + 10^9 * fi sec) $ getTime Monotonic
    where fi = fromIntegral

newTimer :: IO Timer
newTimer = getClock >>= newIORef

measureTimer :: Timer -> IO Integer
measureTimer t = do
    now <- getClock
    atomicModifyIORef t $ \zen -> (now, now - zen)

-- | Print out a summary of site-related statistics
printSiteStats :: SiteState -> IO ()
printSiteStats s@SiteState{..} = do
    let [total, okay, fail] = map IS.size [scrapedMap, presentMap, deletedMap s]

    printf "Scraped count: %d\n" total
    printf "Success count: %d\n" okay
    printf "Deleted count: %d\n" fail
    printf "=> Success ratio: %.2f%%\n" $ 100 * okay // total
    printf "=> Deleted ratio: %.2f%%\n" $ 100 * fail // total
    printf "\n"

    let [seen, know] = map IS.size [authorMap, knownMap]

    printf "Authors seen:  %d\n" seen
    printf "Authors known: %d\n" know
    printf "\n"

    printf "## scrapedMap stats:\n"
    isStats scrapedMap
    printf "\n"

    printf "## presentMap stats:\n"
    isStats presentMap
    printf "\n"

    printf "## authorMap stats:\n"
    isStats authorMap
    printf "\n"

    printf "## knownMap stats:\n"
    isStats knownMap

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
