{-# LANGUAGE TupleSections #-}

import Control.DeepSeq (NFData)
import Streamly.Data.Fold (Fold)
import Streamly.Data.Scanl (Scanl)
import Streamly.Data.Stream (Stream)
import System.Random (randomRIO)

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.RingArray as Ring
import qualified Streamly.Internal.Data.Scanl as Scanl
import qualified Streamly.Data.Stream as Stream
import qualified Streamly.Data.Array as Array
import qualified Streamly.Statistics as Statistics
import qualified Streamly.Statistics.Scanl as StatScan

import Gauge

{-# INLINE source #-}
source :: (Monad m, Num a, Stream.Enumerable a) => Int -> a -> Stream m a
source len from =
    Stream.enumerateFromThenTo from (from + 1) (from + fromIntegral len)

{-# INLINE sourceDescending #-}
sourceDescending :: (Monad m, Num a, Stream.Enumerable a) =>
    Int -> a -> Stream m a
sourceDescending len from =
    Stream.enumerateFromThenTo
        (from + fromIntegral len)
        (from + fromIntegral (len - 1))
        from

{-# INLINE sourceDescendingInt #-}
sourceDescendingInt :: Monad m => Int -> Int -> Stream m Int
sourceDescendingInt = sourceDescending

{-# INLINE benchWith #-}
benchWith :: (Num a, NFData a) =>
    (Int -> a -> Stream IO a) -> Int -> String -> Fold IO a a -> Benchmark
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

{-# INLINE benchWithScanSrc #-}
benchWithScanSrc :: (Num a) =>
    (Int -> a -> Stream IO a) -> Int -> String -> Scanl IO a a -> Benchmark
benchWithScanSrc src len name f =
    bench name
        $ nfIO
        $ randomRIO (1, 1 :: Int)
        >>= Stream.fold Fold.drain
            . Stream.postscanl f . src len . fromIntegral

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Scanl IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>=
    Stream.fold Fold.drain . Stream.postscanl f . source len

{-# INLINE benchWithResample #-}
benchWithResample :: Int -> String -> Benchmark
benchWithResample len name = bench name $ nfIO $ do
    i <- randomRIO (1, 1)
    arr <- Stream.fold Array.create (source len i :: Stream IO Double)
    Stream.fold Fold.drain $ Stream.unfold Statistics.resample arr

{-# INLINE benchWithFoldResamples #-}
benchWithFoldResamples :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithFoldResamples len name f = bench name $ nfIO $ do
    i <- randomRIO (1, 1)
    arr <- Stream.fold Array.create (source len i :: Stream IO Double)
    Stream.fold Fold.drain $ Statistics.foldResamples len arr f

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

mkBenchmarks ::
       (Int -> String -> Fold IO Double Double -> Benchmark)
    -> [Benchmark]
mkBenchmarks mkBench =
    [
      mkBench numElements "minimum (window size 100)"
        (Ring.slidingWindow 100 Statistics.minimum)
    , mkBench numElements "minimum (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.minimum)
    , benchWith sourceDescendingInt numElements
        "minimum descending (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.minimum)

    , mkBench numElements "maximum (window size 100)"
        (Ring.slidingWindow 100 Statistics.maximum)
    , mkBench numElements "maximum (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.maximum)
    , benchWith sourceDescendingInt numElements
        "maximum descending (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.maximum)

    , mkBench numElements "range (window size 100)"
        (Ring.slidingWindow 100 Statistics.range)
    , mkBench numElements "range (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.range)

    , mkBench numElements "sum (window size 100)"
        (Ring.slidingWindow 100 Statistics.sum)
    , mkBench numElements "sum (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.sum)
    , mkBench numElements "sum (entire stream)"
        (Statistics.cumulative Statistics.sum)
    , mkBench numElements "sum (Data.Fold)" Fold.sum

    , mkBench numElements "mean (window size 100)"
        (Ring.slidingWindow 100 Statistics.mean)
    , mkBench numElements "mean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.mean)
    , mkBench numElements "mean (entire stream)"
        (Statistics.cumulative Statistics.mean)
    , mkBench numElements "mean (Data.Fold)" Fold.mean

    , mkBench numElements "welfordMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.welfordMean)
    , mkBench numElements "welfordMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.welfordMean)
    , mkBench numElements "welfordMean (entire stream)"
        (Statistics.cumulative Statistics.welfordMean)

    , mkBench numElements "geometricMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.geometricMean)
    , mkBench numElements "geometricMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.geometricMean)
    , mkBench numElements "geometricMean (entire stream)"
        (Statistics.cumulative Statistics.geometricMean)

    , mkBench numElements "harmonicMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.harmonicMean)
    , mkBench numElements "harmonicMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.harmonicMean)
    , mkBench numElements "harmonicMean (entire stream)"
        (Statistics.cumulative Statistics.harmonicMean)

    , mkBench numElements "quadraticMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.quadraticMean)
    , mkBench numElements "quadraticMean (window size 1000)"
        (Ring.slidingWindow 1000 Statistics.quadraticMean)
    , mkBench numElements "quadraticMean (entire stream)"
        (Statistics.cumulative Statistics.quadraticMean)

    , mkBench numElements "powerSum 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerSum 2))
    , mkBench numElements "powerSum 2 (entire stream)"
        (Statistics.cumulative (Statistics.powerSum 2))

    , mkBench numElements "rawMoment 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerSum 2))
    , mkBench numElements "rawMoment 2 (entire stream)"
        (Statistics.cumulative (Statistics.rawMoment 2))

    , mkBench numElements "powerMean 1 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMean 1))
    , mkBench numElements "powerMean 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMean 2))
    , mkBench numElements "powerMean 10 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMean 10))

    , mkBench numElements "powerMeanFrac (-1) (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac (-1)))
    , mkBench numElements "powerMeanFrac 1 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac 1))
    , mkBench numElements "powerMeanFrac 2 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac 2))
    , mkBench numElements "powerMeanFrac 10 (window size 100)"
        (Ring.slidingWindow 100 (Statistics.powerMeanFrac 10))

    , mkBench numElements "ewma (entire stream)"
        (Statistics.ewma 0.5)
    , mkBench numElements "ewmaAfterMean (entire stream)"
         (Statistics.ewmaAfterMean 10 0.5)
    , mkBench numElements "ewmaRampUpSmoothing (entire stream)"
        (Statistics.ewmaRampUpSmoothing 0.5 0.5)

    , mkBench numElements "variance (window size 100)"
        (Ring.slidingWindow 100 Statistics.variance)
    , mkBench numElements "variance (entire stream)"
        (Statistics.cumulative Statistics.variance)
    -- , mkBench numElements "variance (Data.Fold)" Fold.variance

    , mkBench numElements "sampleVariance (window size 100)"
        (Ring.slidingWindow 100 Statistics.sampleVariance)
    , mkBench numElements "sampleVariance (entire stream)"
        (Statistics.cumulative Statistics.sampleVariance)

    , mkBench numElements "stdDev (window size 100)"
        (Ring.slidingWindow 100 Statistics.stdDev)
    , mkBench numElements "stdDev (entire stream)"
        (Statistics.cumulative Statistics.stdDev)
    -- , mkBench numElements "stdDev (Data.Fold)" Fold.stdDev

    , mkBench numElements "sampleStdDev (window size 100)"
        (Ring.slidingWindow 100 Statistics.sampleStdDev)
    , mkBench numElements "sampleStdDev (entire stream)"
        (Statistics.cumulative Statistics.sampleStdDev)

    , mkBench numElements "stdErrMean (window size 100)"
        (Ring.slidingWindow 100 Statistics.stdErrMean)
    , mkBench numElements "stdErrMean (entire stream)"
        (Statistics.cumulative Statistics.stdErrMean)

-- These benchmarks take a lot of time/memory with fusion-plugin possibly
-- because of the use of Tee.
#ifndef FUSION_PLUGIN
    , mkBench numElements "skewness (window size 100)"
        (Ring.slidingWindow 100 Statistics.skewness)
    , mkBench numElements "skewness (entire stream)"
        (Statistics.cumulative Statistics.skewness)

    , mkBench numElements "kurtosis (window size 100)"
        (Ring.slidingWindow 100 Statistics.kurtosis)
    , mkBench numElements "kurtosis (entire stream)"
        (Statistics.cumulative Statistics.kurtosis)
#endif
    , mkBench numElements "md (window size 100)"
        (Ring.slidingWindowWith 100 Statistics.md)

    ]

mkScans ::
       (Int -> String -> Scanl IO Double Double -> Benchmark)
    -> [Benchmark]
mkScans mkBench =
    [
      mkBench numElements "minimum (window size 100)"
        (Scanl.incrScan 100 StatScan.incrMinimum)
    , mkBench numElements "minimum (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrMinimum)
    , benchWithScanSrc sourceDescendingInt numElements
        "minimum descending (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrMinimum)

    , mkBench numElements "maximum (window size 100)"
        (Scanl.incrScan 100 StatScan.incrMaximum)
    , mkBench numElements "maximum (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrMaximum)
    , benchWithScanSrc sourceDescendingInt numElements
        "maximum descending (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrMaximum)

    , mkBench numElements "range (window size 100)"
        (Scanl.incrScan 100 StatScan.incrRange)
    , mkBench numElements "range (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrRange)

    , mkBench numElements "sum (window size 100)"
        (Scanl.incrScan 100 Scanl.incrSum)
    , mkBench numElements "sum (window size 1000)"
        (Scanl.incrScan 1000 Scanl.incrSum)
    , mkBench numElements "sum (entire stream)"
        (Scanl.cumulativeScan Scanl.incrSum)
    , mkBench numElements "sum (Data.Fold)" Scanl.sum

    , mkBench numElements "mean (window size 100)"
        (Scanl.incrScan 100 Scanl.incrMean)
    , mkBench numElements "mean (window size 1000)"
        (Scanl.incrScan 1000 Scanl.incrMean)
    , mkBench numElements "mean (entire stream)"
        (Scanl.cumulativeScan Scanl.incrMean)
    , mkBench numElements "mean (Data.Fold)" Scanl.mean

    , mkBench numElements "welfordMean (window size 100)"
        (Scanl.incrScan 100 StatScan.incrWelfordMean)
    , mkBench numElements "welfordMean (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrWelfordMean)
    , mkBench numElements "welfordMean (entire stream)"
        (Scanl.cumulativeScan StatScan.incrWelfordMean)

    , mkBench numElements "geometricMean (window size 100)"
        (Scanl.incrScan 100 StatScan.incrGeometricMean)
    , mkBench numElements "geometricMean (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrGeometricMean)
    , mkBench numElements "geometricMean (entire stream)"
        (Scanl.cumulativeScan StatScan.incrGeometricMean)

    , mkBench numElements "harmonicMean (window size 100)"
        (Scanl.incrScan 100 StatScan.incrHarmonicMean)
    , mkBench numElements "harmonicMean (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrHarmonicMean)
    , mkBench numElements "harmonicMean (entire stream)"
        (Scanl.cumulativeScan StatScan.incrHarmonicMean)

    , mkBench numElements "quadraticMean (window size 100)"
        (Scanl.incrScan 100 StatScan.incrQuadraticMean)
    , mkBench numElements "quadraticMean (window size 1000)"
        (Scanl.incrScan 1000 StatScan.incrQuadraticMean)
    , mkBench numElements "quadraticMean (entire stream)"
        (Scanl.cumulativeScan StatScan.incrQuadraticMean)

    , mkBench numElements "powerSum 2 (window size 100)"
        (Scanl.incrScan 100 (Scanl.incrPowerSum 2))
    , mkBench numElements "powerSum 2 (entire stream)"
        (Scanl.cumulativeScan (Scanl.incrPowerSum 2))

    , mkBench numElements "rawMoment 2 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrRawMoment 2))
    , mkBench numElements "rawMoment 2 (entire stream)"
        (Scanl.cumulativeScan (StatScan.incrRawMoment 2))

    , mkBench numElements "powerMean 1 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMean 1))
    , mkBench numElements "powerMean 2 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMean 2))
    , mkBench numElements "powerMean 10 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMean 10))

    , mkBench numElements "powerMeanFrac (-1) (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMeanFrac (-1)))
    , mkBench numElements "powerMeanFrac 1 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMeanFrac 1))
    , mkBench numElements "powerMeanFrac 2 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMeanFrac 2))
    , mkBench numElements "powerMeanFrac 10 (window size 100)"
        (Scanl.incrScan 100 (StatScan.incrPowerMeanFrac 10))

    , mkBench numElements "ewma (entire stream)"
        (StatScan.ewma 0.5)
    , mkBench numElements "ewmaRampUpSmoothing (entire stream)"
        (StatScan.ewmaRampUpSmoothing 0.5 0.5)

    , mkBench numElements "variance (window size 100)"
        (Scanl.incrScan 100 StatScan.incrVariance)
    , mkBench numElements "variance (entire stream)"
        (Scanl.cumulativeScan StatScan.incrVariance)
    -- , mkBench numElements "variance (Data.Fold)" Fold.variance

    , mkBench numElements "sampleVariance (window size 100)"
        (Scanl.incrScan 100 StatScan.incrSampleVariance)
    , mkBench numElements "sampleVariance (entire stream)"
        (Scanl.cumulativeScan StatScan.incrSampleVariance)

    , mkBench numElements "stdDev (window size 100)"
        (Scanl.incrScan 100 StatScan.incrStdDev)
    , mkBench numElements "stdDev (entire stream)"
        (Scanl.cumulativeScan StatScan.incrStdDev)
    -- , mkBench numElements "stdDev (Data.Fold)" Fold.stdDev

    , mkBench numElements "sampleStdDev (window size 100)"
        (Scanl.incrScan 100 StatScan.incrSampleStdDev)
    , mkBench numElements "sampleStdDev (entire stream)"
        (Scanl.cumulativeScan StatScan.incrSampleStdDev)

    , mkBench numElements "stdErrMean (window size 100)"
        (Scanl.incrScan 100 StatScan.incrStdErrMean)
    , mkBench numElements "stdErrMean (entire stream)"
        (Scanl.cumulativeScan StatScan.incrStdErrMean)

-- These benchmarks take a lot of time/memory with fusion-plugin possibly
-- because of the use of Tee.
#ifndef FUSION_PLUGIN
    , mkBench numElements "skewness (window size 100)"
        (Scanl.incrScan 100 StatScan.windowSkewness)
    , mkBench numElements "skewness (entire stream)"
        (Scanl.cumulativeScan StatScan.windowSkewness)

    , mkBench numElements "kurtosis (window size 100)"
        (Scanl.incrScan 100 StatScan.windowKurtosis)
    , mkBench numElements "kurtosis (entire stream)"
        (Scanl.cumulativeScan StatScan.windowKurtosis)
#endif
    , mkBench numElements "md (window size 100)"
        (Scanl.incrScanWith 100 StatScan.incrMd)

    ]

main :: IO ()
main =
  defaultMain
    [
      bgroup "fold" $ mkBenchmarks benchWithFold
    , bgroup "fold_Int"
        [ benchWithFoldInt numElements "sumInt (window size 100)"
            (Ring.slidingWindow 100 Statistics.sumInt)
        , benchWithFoldInt numElements "sum for Int (window size 100)"
            (Ring.slidingWindow 100 Statistics.sum)
        ]
    , bgroup "scan" $ mkScans benchWithPostscan
    -- XXX These benchmarks measure the cost of creating the array as well,
    -- we can do that outside the benchmark.
    , bgroup "resample"
        [ benchWithResample numElements "Resample"
        , benchWithFoldResamples 316 "FoldResamples 316" Fold.mean
        ]
    ]
