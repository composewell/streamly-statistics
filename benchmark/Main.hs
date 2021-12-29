{-# LANGUAGE TupleSections #-}
import Gauge

import System.Random (randomRIO)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Statistics as Statistics
import qualified Streamly.Prelude as S
import qualified Streamly.Internal.Data.Ring.Foreign as Ring

{-# INLINE source #-}
source :: (Monad m, S.IsStream t) => Int -> Int -> t m Double
source len from =
  S.enumerateFromTo (fromIntegral from) (fromIntegral (from + len))

{-# INLINE benchWithFold #-}
benchWithFold :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFold len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.fold f . source len

{-# INLINE benchWithScan #-}
benchWithScan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithScan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.drain . S.scan f . source len

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.drain . S.postscan f . source len

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

{-# INLINE windowSize #-}
windowSize :: Statistics.WindowSize
windowSize = Statistics.Finite 100

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fold"
        [ benchWithFold numElements "min (window size 100)"
            (Ring.slidingWindow 100 Statistics.min)
        , benchWithFold numElements "min (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.min)
        , benchWithFold numElements "max (window size 100)"
            (Ring.slidingWindow 100 Statistics.max)
        , benchWithFold numElements "max (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.max)
        , benchWithFold numElements "range (window size 100)"
            (Ring.slidingWindow 100 Statistics.range)
        , benchWithFold numElements "range (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.range)
        , benchWithFold numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Statistics.sum)
        , benchWithFold numElements "sum (window size 1000"
            (Ring.slidingWindow 1000 Statistics.sum)
        , benchWithFold numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Statistics.mean)
        , benchWithFold numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.mean)
        , benchWithFold
            numElements
            "welfordMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.welfordMean)
        , benchWithFold
            numElements
            "welfordMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.welfordMean)

        ]
    , bgroup
        "scan"
        [ benchWithPostscan numElements "min (window size 100)"
            (Ring.slidingWindow 100 Statistics.min)
        , benchWithPostscan numElements "min (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.min)
        , benchWithPostscan numElements "max (window size 100)"
            (Ring.slidingWindow 100 Statistics.max)
        , benchWithPostscan numElements "max (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.max)
        , benchWithPostscan numElements "range (window size 100)"
            (Ring.slidingWindow 100 Statistics.range)
        , benchWithPostscan numElements "range (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.range)
        , benchWithPostscan numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Statistics.sum)
        , benchWithPostscan numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.sum)
        , benchWithPostscan numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Statistics.mean)
        , benchWithPostscan numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.mean)
        , benchWithPostscan
            numElements
            "welfordMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.welfordMean)
        , benchWithPostscan
            numElements
            "welfordMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.welfordMean)

        ]
    ]
