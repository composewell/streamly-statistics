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

runBenchmarks ::
       (Int -> String -> Fold IO Double Double -> Benchmark)
    -> [Benchmark]
runBenchmarks fDouble =
    [
      fDouble numElements "min (window size 100)"
        (Ring.slidingWindow 100 Statistics.minimum)
    , fDouble numElements "min (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.minimum)
    , benchWith sourceDescendingInt numElements
        "min descending (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.minimum)

    , fDouble numElements "max (window size 100)"
        (Ring.slidingWindow 100 Statistics.maximum)
    , fDouble numElements "max (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.maximum)
    , benchWith sourceDescendingInt numElements
        "max descending (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.maximum)

    , fDouble numElements "range (window size 100)"
        (Ring.slidingWindow 100 Statistics.range)
    , fDouble numElements "range (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.range)

    , fDouble numElements "sum (window size 100)"
        (Ring.slidingWindow 100 Statistics.sum)
    , fDouble numElements "sum (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.sum)
    , fDouble numElements "sum (entire stream)"
        (Statistics.cumulative Statistics.sum)
    , fDouble numElements "sum (Data.Fold)" Fold.sum
    , fDouble numElements "mean (window size 100)"
        (Ring.slidingWindow 100 Statistics.mean)
    , fDouble numElements "mean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.mean)
    , fDouble numElements "mean (entire stream)"
        (Statistics.cumulative Statistics.mean)
    , fDouble numElements "mean (Data.Fold)" Fold.mean

    , fDouble numElements "welfordMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.welfordMean)
    , fDouble numElements "welfordMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.welfordMean)
    , fDouble numElements "welfordMean (entire stream)"
        (Statistics.cumulative Statistics.welfordMean)

    , fDouble numElements "geometricMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.geometricMean)
    , fDouble numElements "geometricMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.geometricMean)
    , fDouble numElements "geometricMean (entire stream)"
        (Statistics.cumulative Statistics.geometricMean)

    , fDouble numElements "harmonicMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.harmonicMean)
    , fDouble numElements "harmonicMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.harmonicMean)
    , fDouble numElements "harmonicMean (entire stream)"
        (Statistics.cumulative Statistics.harmonicMean)

    , fDouble numElements "quadraticMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.quadraticMean)
    , fDouble numElements "quadraticMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.quadraticMean)
    , fDouble numElements "quadraticMean (entire stream)"
        (Statistics.cumulative Statistics.quadraticMean)

    , fDouble numElements "powerSum 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerSum 2))
    , fDouble numElements "powerSum 2 (entire stream)"
        (Statistics.cumulative (Statistics.powerSum 2))

    , fDouble numElements "rawMoment 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerSum 2))
    , fDouble numElements "rawMoment 2 (entire stream)"
        (Statistics.cumulative (Statistics.rawMoment 2))

    , fDouble numElements "powerMean 1 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMean 1))
    , fDouble numElements "powerMean 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMean 2))
    , fDouble numElements "powerMean 10 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMean 10))

    , fDouble numElements "powerMeanFrac (-1) (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac (-1)))
    , fDouble numElements "powerMeanFrac 1 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac 1))
    , fDouble numElements "powerMeanFrac 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac 2))
    , fDouble numElements "powerMeanFrac 10 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac 10))

    , fDouble numElements "ewma (entire stream)"
        (Statistics.ewma 0.5)
    , fDouble numElements "ewmaAfterMean (entire stream)"
        (Statistics.ewmaAfterMean 10 0.5)
    , fDouble numElements "ewmaRampUpSmoothing (entire stream)"
        (Statistics.ewmaRampUpSmoothing 0.5 0.5)

    , fDouble numElements "variance (window size 100)"
        (Ring.slidingWindow 100 Statistics.variance)
    , fDouble numElements "variance (entire stream)"
        (Statistics.cumulative Statistics.variance)
    , fDouble numElements "variance (Data.Fold)" Fold.variance

    , fDouble numElements "sampleVariance (window size 100)"
        (Ring.slidingWindow 100 Statistics.sampleVariance)
    , fDouble numElements "sampleVariance (entire stream)"
        (Statistics.cumulative Statistics.sampleVariance)

    , fDouble numElements "stdDev (window size 100)"
        (Ring.slidingWindow 100 Statistics.stdDev)
    , fDouble numElements "stdDev (entire stream)"
        (Statistics.cumulative Statistics.stdDev)
    , fDouble numElements "stdDev (Data.Fold)" Fold.stdDev

    , fDouble numElements "sampleStdDev (window size 100)"
        (Ring.slidingWindow 100 Statistics.sampleStdDev)
    , fDouble numElements "sampleStdDev (entire stream)"
        (Statistics.cumulative Statistics.sampleStdDev)

    , fDouble numElements "stdErrMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.stdErrMean)
    , fDouble numElements "stdErrMean (entire stream)"
        (Statistics.cumulative Statistics.stdErrMean)

-- These benchmarks take a lot of time/memory with fusion-plugin possibly
-- because of the use of Tee.
#ifndef FUSION_PLUGIN
    , fDouble numElements "skewness (window size 100)"
        (Ring.slidingWindow 100 Statistics.skewness)
    , fDouble numElements "skewness (entire stream)"
        (Statistics.cumulative Statistics.skewness)

    , fDouble numElements "kurtosis (window size 100)"
        (Ring.slidingWindow 100 Statistics.kurtosis)
    , fDouble numElements "kurtosis (entire stream)"
        (Statistics.cumulative Statistics.kurtosis)
#endif
    , fDouble numElements "md (window size 100)"
        (Ring.slidingWindowWith 100 Statistics.md)

    ]

main :: IO ()
main =
  defaultMain
    [
      bgroup "fold" $ runBenchmarks benchWithFold
    , bgroup "fold_Int"
        [ benchWithFoldInt numElements "sumInt (window size 100)"
            (Ring.slidingWindow 100 Statistics.sumInt)
        , benchWithFoldInt numElements "sum for Int (window size 100)"
            (Ring.slidingWindow 100 Statistics.sum)
        ]
    , bgroup "scan" $ runBenchmarks benchWithPostscan
    -- XXX These benchmarks measure the cost of creating the array as well,
    -- we can do that outside the benchmark.
    , bgroup "resample"
        [ benchWithResample numElements "Resample"
        , benchWithFoldResamples 316 "FoldResamples 316" Fold.mean
        ]
    ]
