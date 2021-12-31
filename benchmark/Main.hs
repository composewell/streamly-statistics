{-# LANGUAGE TupleSections #-}

import Gauge
import Streamly.Data.Fold (Fold)
import System.Random (randomRIO)

import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Prelude as Stream
import qualified Streamly.Statistics as Statistics

{-# INLINE source #-}
source :: (Monad m, Stream.IsStream t) => Int -> Int -> t m Double
source len from =
    Stream.enumerateFromTo (fromIntegral from) (fromIntegral (from + len))

{-# INLINE benchWithFold #-}
benchWithFold :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFold len name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= Stream.fold f . source len

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>=
    Stream.drain . Stream.postscan f . source len

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

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
