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

{-# INLINE benchWithFoldInf #-}
benchWithFoldInf :: Int -> String -> Fold IO (Double, Maybe Double) Double -> Benchmark
benchWithFoldInf len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.fold f . S.map (, Nothing) . source len

{-# INLINE benchWithScan #-}
benchWithScan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithScan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.drain . S.scan f . source len

{-# INLINE benchWithPostscan #-}
benchWithPostscan :: Int -> String -> Fold IO Double Double -> Benchmark
benchWithPostscan len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.drain . S.postscan f . source len

{-# INLINE benchWithPostscanInf #-}
benchWithPostscanInf :: Int -> String -> Fold IO (Double, Maybe Double) Double -> Benchmark
benchWithPostscanInf len name f =
  bench name $ nfIO $ randomRIO (1, 1) >>= S.drain . S.postscan f . S.map (, Nothing) . source len

{-# INLINE numElements #-}
numElements :: Int
numElements = 100000

windowSizeVal = 100

{-# INLINE windowSize #-}
windowSize :: Statistics.WindowSize
windowSize = Statistics.Finite windowSizeVal

main :: IO ()
main =
  defaultMain
    [ bgroup
        "finite"
        [ bgroup
            "fold"
            [ benchWithFold numElements "min" (Ring.slidingWindow windowSizeVal Statistics.min)
            , benchWithFold numElements "max" (Ring.slidingWindow windowSizeVal Statistics.max)
            , benchWithFold numElements "range" (Ring.slidingWindow windowSizeVal Statistics.range)
            , benchWithFold numElements "sum" (Statistics.sum windowSize)
            , benchWithFold numElements "mean" (Statistics.mean windowSize)
            , benchWithFold
                numElements
                "welfordMean"
                (Statistics.welfordMean windowSize)
            ]
        , bgroup
            "scan"
            [ benchWithPostscan numElements "min" (Ring.slidingWindow windowSizeVal Statistics.min)
            , benchWithPostscan numElements "max" (Ring.slidingWindow windowSizeVal Statistics.max)
            , benchWithPostscan numElements "range" (Ring.slidingWindow windowSizeVal Statistics.range)
            , benchWithScan numElements "sum" (Statistics.sum windowSize)
            , benchWithScan numElements "mean" (Statistics.mean windowSize)
            , benchWithScan
                numElements
                "welfordMean"
                (Statistics.welfordMean windowSize)
            ]
        ]
    , bgroup
        "infinite"
        [ bgroup
            "fold"
            [ benchWithFoldInf numElements "min" Statistics.min
            , benchWithFoldInf numElements "max" Statistics.max
            , benchWithFoldInf numElements "range" Statistics.range
            , benchWithFold numElements "sum" (Statistics.sum Statistics.Infinite)
            , benchWithFold numElements "mean" (Statistics.mean Statistics.Infinite)
            , benchWithFold
                numElements
                "welfordMean"
                (Statistics.welfordMean Statistics.Infinite)
            ]
        , bgroup
            "scan"
            [ benchWithPostscanInf numElements "min" Statistics.min
            , benchWithPostscanInf numElements "max" Statistics.max
            , benchWithPostscanInf numElements "range" Statistics.range
            , benchWithScan numElements "sum" (Statistics.sum Statistics.Infinite)
            , benchWithScan numElements "mean" (Statistics.mean Statistics.Infinite)
            , benchWithScan
                numElements
                "welfordMean"
                (Statistics.welfordMean Statistics.Infinite)
            ]
        ]
    ]
