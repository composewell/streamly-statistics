import Gauge

import System.Random (randomRIO)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Statistics as Statistics
import qualified Streamly.Prelude as S

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
        "finite"
        [ bgroup
            "fold"
            [ benchWithFold numElements "min" (Statistics.min windowSize)
            , benchWithFold numElements "max" (Statistics.max windowSize)
            , benchWithFold numElements "range" (Statistics.range windowSize)
            , benchWithFold numElements "sum" (Statistics.sum windowSize)
            , benchWithFold numElements "mean" (Statistics.mean windowSize)
            , benchWithFold
                numElements
                "welfordMean"
                (Statistics.welfordMean windowSize)
            ]
        , bgroup
            "scan"
            [ benchWithScan numElements "min" (Statistics.min windowSize)
            , benchWithScan numElements "max" (Statistics.max windowSize)
            , benchWithScan numElements "range" (Statistics.range windowSize)
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
            [ benchWithFold numElements "min" (Statistics.min Statistics.Infinite)
            , benchWithFold numElements "max" (Statistics.max Statistics.Infinite)
            , benchWithFold numElements "range" (Statistics.range Statistics.Infinite)
            , benchWithFold numElements "sum" (Statistics.sum Statistics.Infinite)
            , benchWithFold numElements "mean" (Statistics.mean Statistics.Infinite)
            , benchWithFold
                numElements
                "welfordMean"
                (Statistics.welfordMean Statistics.Infinite)
            ]
        , bgroup
            "scan"
            [ benchWithScan numElements "min" (Statistics.min Statistics.Infinite)
            , benchWithScan numElements "max" (Statistics.max Statistics.Infinite)
            , benchWithScan numElements "range" (Statistics.range Statistics.Infinite)
            , benchWithScan numElements "sum" (Statistics.sum Statistics.Infinite)
            , benchWithScan numElements "mean" (Statistics.mean Statistics.Infinite)
            , benchWithScan
                numElements
                "welfordMean"
                (Statistics.welfordMean Statistics.Infinite)
            ]
        ]
    ]
