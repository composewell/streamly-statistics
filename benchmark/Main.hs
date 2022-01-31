{-# LANGUAGE TupleSections #-}

import Gauge
import Streamly.Data.Fold (Fold)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Prelude as Stream
import qualified Streamly.Statistics as Statistics

{-# INLINE source #-}
source :: (Monad m, Stream.IsStream t, Num a, Stream.Enumerable a) =>
    a -> a -> t m a
source len from = Stream.enumerateFromTo from (from + len)

{-# INLINE benchWithFold #-}
benchWithFold :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFold len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1) >>= Stream.fold f . source (fromIntegral len)

{-# INLINE benchWithFoldInt #-}
benchWithFoldInt :: Int -> String -> Fold IO Int Int -> Benchmark
benchWithFoldInt len name f =
    bench name $ nfIO $ randomRIO (1, 1) >>= Stream.fold f . source len

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>=
    Stream.drain . Stream.postscan f . source (fromIntegral len)

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

        , benchWithFoldInt numElements "sumInt (window size 100)"
            (Ring.slidingWindow 100 Statistics.sumInt)
        , benchWithFoldInt numElements "sum for Int (window size 100)"
            (Ring.slidingWindow 100 Statistics.sum)

        , benchWithFold numElements "sum (window size 100)"
            (Ring.slidingWindow 100 Statistics.sum)
        , benchWithFold numElements "sum (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.sum)
        , benchWithFold numElements "sum (entire stream)"
            (Statistics.noSlide Statistics.sum)
        , benchWithFold numElements "sum (Data.Fold)"
            (Fold.sum)

        , benchWithFold numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Statistics.mean)
        , benchWithFold numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.mean)
        , benchWithFold numElements "mean (entire stream)"
            (Statistics.noSlide Statistics.mean)
        , benchWithFold numElements "mean (Data.Fold)"
            (Fold.mean)

        , benchWithFold
            numElements
            "welfordMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.welfordMean)
        , benchWithFold
            numElements
            "welfordMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.welfordMean)
        , benchWithFold
            numElements
            "welfordMean (entire stream)"
            (Statistics.noSlide Statistics.welfordMean)

        , benchWithFold numElements "geometricMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.geometricMean)
        , benchWithFold numElements "geometricMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.geometricMean)
        , benchWithFold numElements "geometricMean (entire stream)"
            (Statistics.noSlide Statistics.geometricMean)

        , benchWithFold numElements "harmonicMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.harmonicMean)
        , benchWithFold numElements "harmonicMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.harmonicMean)
        , benchWithFold numElements "harmonicMean (entire stream)"
            (Statistics.noSlide Statistics.harmonicMean)

        , benchWithFold numElements "quadraticMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.quadraticMean)
        , benchWithFold numElements "quadraticMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.quadraticMean)
        , benchWithFold numElements "quadraticMean (entire stream)"
            (Statistics.noSlide Statistics.quadraticMean)

        , benchWithFold numElements "powerSum 2 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerSum 2))
        , benchWithFold numElements "powerSum 2 (entire stream)"
            (Statistics.noSlide (Statistics.powerSum 2))

        , benchWithFold numElements "rawMoment 2 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerSum 2))
        , benchWithFold numElements "rawMoment 2 (entire stream)"
            (Statistics.noSlide (Statistics.rawMoment 2))

        , benchWithFold numElements "powerMean 1 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMean 1))
        , benchWithFold numElements "powerMean 2 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMean 2))
        , benchWithFold numElements "powerMean 10 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMean 10))

        , benchWithFold numElements "powerMeanFrac (-1) (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMeanFrac (-1)))
        , benchWithFold numElements "powerMeanFrac 1 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMeanFrac 1))
        , benchWithFold numElements "powerMeanFrac 2 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMeanFrac 2))
        , benchWithFold numElements "powerMeanFrac 10 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerMeanFrac 10))

        , benchWithFold numElements "variance (window size 100)"
            (Ring.slidingWindow 100 (Statistics.variance))
        , benchWithFold numElements "variance (entire stream)"
            (Statistics.noSlide (Statistics.variance))
        , benchWithFold numElements "variance (Data.Fold)"
            (Fold.variance)

        , benchWithFold numElements "sampleVariance (window size 100)"
            (Ring.slidingWindow 100 (Statistics.sampleVariance))
        , benchWithFold numElements "sampleVariance (entire stream)"
            (Statistics.noSlide (Statistics.sampleVariance))

        , benchWithFold numElements "stdDev (window size 100)"
            (Ring.slidingWindow 100 (Statistics.stdDev))
        , benchWithFold numElements "stdDev (entire stream)"
            (Statistics.noSlide (Statistics.stdDev))
        , benchWithFold numElements "stdDev (Data.Fold)"
            (Fold.stdDev)

        , benchWithFold numElements "sampleStdDev (window size 100)"
            (Ring.slidingWindow 100 (Statistics.sampleStdDev))
        , benchWithFold numElements "sampleStdDev (entire stream)"
            (Statistics.noSlide (Statistics.sampleStdDev))

        , benchWithFold numElements "stdErrMean (window size 100)"
            (Ring.slidingWindow 100 (Statistics.stdErrMean))
        , benchWithFold numElements "stdErrMean (entire stream)"
            (Statistics.noSlide (Statistics.stdErrMean))

        , benchWithFold numElements "skewness (window size 100)"
            (Ring.slidingWindow 100 (Statistics.skewness))
        , benchWithFold numElements "skewness (entire stream)"
            (Statistics.noSlide (Statistics.skewness))

        , benchWithFold numElements "kurtosis (window size 100)"
            (Ring.slidingWindow 100 (Statistics.kurtosis))
        , benchWithFold numElements "kurtosis (entire stream)"
            (Statistics.noSlide (Statistics.kurtosis))
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
