{-# LANGUAGE TupleSections #-}

import Control.DeepSeq (NFData)
import Streamly.Data.Fold (Fold)
import Streamly.Prelude (SerialT)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Internal.Data.Stream.IsStream.Type as S
import qualified Streamly.Prelude as Stream
import qualified Streamly.Statistics as Statistics

import Gauge

{-# INLINE source #-}
source :: (Monad m, Stream.IsStream t, Num a, Stream.Enumerable a) =>
    Int -> a -> t m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE sourceDescending #-}
sourceDescending :: (Monad m, Stream.IsStream t, Num a, Stream.Enumerable a) =>
    Int -> a -> t m a
sourceDescending len from =
    Stream.enumerateFromThenTo
        (from + fromIntegral len)
        (from + fromIntegral (len - 1))
        from

{-# INLINE sourceDescendingInt #-}
sourceDescendingInt :: (Monad m, Stream.IsStream t) => Int -> Int -> t m Int
sourceDescendingInt = sourceDescending

{-# INLINE benchWith #-}
benchWith :: (Num a, NFData a) =>
    (Int -> a -> SerialT IO a) -> Int -> String -> Fold IO a a -> Benchmark
benchWith src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int) >>= Stream.fold f . src len . fromIntegral

{-# INLINE benchWithFold #-}
benchWithFold :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFold len name f = benchWith source len name f

{-# INLINE benchWithFoldInt #-}
benchWithFoldInt :: Int -> String -> Fold IO Int Int -> Benchmark
benchWithFoldInt len name f = benchWith source len name f

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>=
    Stream.drain . Stream.postscan f . source len

{-# INLINE benchWithResample #-}
benchWithResample :: Int -> String -> Benchmark
benchWithResample len name = bench name $ nfIO $ do
    i <- randomRIO (1, 1)
    arr <- Array.fromStreamD (S.toStreamD (source len i :: SerialT IO Double))
    Stream.drain $ Stream.unfold Statistics.resample arr

{-# INLINE benchWithFoldResamples #-}
benchWithFoldResamples :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFoldResamples len name f = bench name $ nfIO $ do
    i <- randomRIO (1, 1)
    arr <- Array.fromStreamD (S.toStreamD (source len i :: SerialT IO Double))
    Stream.drain $ Statistics.foldResamples len arr f

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

main :: IO ()
main =
  defaultMain
    [ bgroup
        "fold"
        [ benchWithFold numElements "minimum (window size 100)"
            (Ring.slidingWindow 100 Statistics.minimum)
        , benchWithFold numElements "minimum (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.minimum)
        , benchWith sourceDescendingInt numElements
            "minimum descending (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.minimum)

        , benchWithFold numElements "maximum (window size 100)"
            (Ring.slidingWindow 100 Statistics.maximum)
        , benchWithFold numElements "maximum (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.maximum)
        , benchWith sourceDescendingInt numElements
            "maximum descending (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.maximum)

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
            (Statistics.cumulative Statistics.sum)
        , benchWithFold numElements "sum (Data.Fold)"
            (Fold.sum)

        , benchWithFold numElements "mean (window size 100)"
            (Ring.slidingWindow 100 Statistics.mean)
        , benchWithFold numElements "mean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.mean)
        , benchWithFold numElements "mean (entire stream)"
            (Statistics.cumulative Statistics.mean)
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
            (Statistics.cumulative Statistics.welfordMean)

        , benchWithFold numElements "geometricMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.geometricMean)
        , benchWithFold numElements "geometricMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.geometricMean)
        , benchWithFold numElements "geometricMean (entire stream)"
            (Statistics.cumulative Statistics.geometricMean)

        , benchWithFold numElements "harmonicMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.harmonicMean)
        , benchWithFold numElements "harmonicMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.harmonicMean)
        , benchWithFold numElements "harmonicMean (entire stream)"
            (Statistics.cumulative Statistics.harmonicMean)

        , benchWithFold numElements "quadraticMean (window size 100)"
            (Ring.slidingWindow 100 Statistics.quadraticMean)
        , benchWithFold numElements "quadraticMean (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.quadraticMean)
        , benchWithFold numElements "quadraticMean (entire stream)"
            (Statistics.cumulative Statistics.quadraticMean)

        , benchWithFold numElements "powerSum 2 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerSum 2))
        , benchWithFold numElements "powerSum 2 (entire stream)"
            (Statistics.cumulative (Statistics.powerSum 2))

        , benchWithFold numElements "rawMoment 2 (window size 100)"
            (Ring.slidingWindow 100 (Statistics.powerSum 2))
        , benchWithFold numElements "rawMoment 2 (entire stream)"
            (Statistics.cumulative (Statistics.rawMoment 2))

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

        , benchWithFold numElements "ewma (entire stream)"
            (Statistics.ewma 0.5)
        , benchWithFold numElements "ewmaAfterMean (entire stream)"
            (Statistics.ewmaAfterMean 10 0.5)
        , benchWithFold numElements "ewmaRampUpSmoothing (entire stream)"
            (Statistics.ewmaRampUpSmoothing 0.5 0.5)

        , benchWithFold numElements "variance (window size 100)"
            (Ring.slidingWindow 100 (Statistics.variance))
        , benchWithFold numElements "variance (entire stream)"
            (Statistics.cumulative (Statistics.variance))
        , benchWithFold numElements "variance (Data.Fold)"
            (Fold.variance)

        , benchWithFold numElements "sampleVariance (window size 100)"
            (Ring.slidingWindow 100 (Statistics.sampleVariance))
        , benchWithFold numElements "sampleVariance (entire stream)"
            (Statistics.cumulative (Statistics.sampleVariance))

        , benchWithFold numElements "stdDev (window size 100)"
            (Ring.slidingWindow 100 (Statistics.stdDev))
        , benchWithFold numElements "stdDev (entire stream)"
            (Statistics.cumulative (Statistics.stdDev))
        , benchWithFold numElements "stdDev (Data.Fold)"
            (Fold.stdDev)

        , benchWithFold numElements "sampleStdDev (window size 100)"
            (Ring.slidingWindow 100 (Statistics.sampleStdDev))
        , benchWithFold numElements "sampleStdDev (entire stream)"
            (Statistics.cumulative (Statistics.sampleStdDev))

        , benchWithFold numElements "stdErrMean (window size 100)"
            (Ring.slidingWindow 100 (Statistics.stdErrMean))
        , benchWithFold numElements "stdErrMean (entire stream)"
            (Statistics.cumulative (Statistics.stdErrMean))

-- These benchmarks take a lot of time/memory with fusion-plugin possibly
-- because of the use of Tee.
#ifndef FUSION_PLUGIN
        , benchWithFold numElements "skewness (window size 100)"
            (Ring.slidingWindow 100 (Statistics.skewness))
        , benchWithFold numElements "skewness (entire stream)"
            (Statistics.cumulative (Statistics.skewness))

        , benchWithFold numElements "kurtosis (window size 100)"
            (Ring.slidingWindow 100 (Statistics.kurtosis))
        , benchWithFold numElements "kurtosis (entire stream)"
            (Statistics.cumulative (Statistics.kurtosis))
#endif
        , benchWithFold numElements "md (window size 100)"
            (Ring.slidingWindowWith 100 Statistics.md)
        ]
    , bgroup
        "scan"
        [ benchWithPostscan numElements "minimum (window size 100)"
            (Ring.slidingWindow 100 Statistics.minimum)
        , benchWithPostscan numElements "minimum (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.minimum)
        , benchWithPostscan numElements "maximum (window size 100)"
            (Ring.slidingWindow 100 Statistics.maximum)
        , benchWithPostscan numElements "maximum (window size 1000)"
            (Ring.slidingWindow 1000 Statistics.maximum)
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
        , benchWithPostscan
            numElements
            "md (window size 100)"
            (Ring.slidingWindowWith 100 Statistics.md)
        -- XXX These benchmarks measure the cost of creating the array as well,
        -- we can do that outside the benchmark.
        , benchWithResample numElements "Resample"
        , benchWithFoldResamples 316 "FoldResamples 316" Fold.mean
        ]
    ]
