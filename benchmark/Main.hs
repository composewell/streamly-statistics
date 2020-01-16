import Gauge

import Streamly

import System.Random (randomRIO)
import Streamly.Data.Fold (Fold)

import qualified Streamly.Stats as SS
import qualified Streamly.Prelude as S

{-# INLINE source #-}
source :: (Monad m, IsStream t) => Int -> Int -> t m Double
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
windowSize :: SS.WindowSize
windowSize = SS.Finite 100

main :: IO ()
main =
  defaultMain
    [ bgroup
        "finite"
        [ bgroup
            "fold"
            [ benchWithFold numElements "min" (SS.min windowSize)
            , benchWithFold numElements "max" (SS.max windowSize)
            , benchWithFold numElements "range" (SS.range windowSize)
            , benchWithFold numElements "sum" (SS.sum windowSize)
            , benchWithFold numElements "mean" (SS.mean windowSize)
            , benchWithFold
                numElements
                "welfordMean"
                (SS.welfordMean windowSize)
            ]
        , bgroup
            "scan"
            [ benchWithScan numElements "min" (SS.min windowSize)
            , benchWithScan numElements "max" (SS.max windowSize)
            , benchWithScan numElements "range" (SS.range windowSize)
            , benchWithScan numElements "sum" (SS.sum windowSize)
            , benchWithScan numElements "mean" (SS.mean windowSize)
            , benchWithScan
                numElements
                "welfordMean"
                (SS.welfordMean windowSize)
            ]
        ]
    , bgroup
        "infinite"
        [ bgroup
            "fold"
            [ benchWithFold numElements "min" (SS.min SS.Infinite)
            , benchWithFold numElements "max" (SS.max SS.Infinite)
            , benchWithFold numElements "range" (SS.range SS.Infinite)
            , benchWithFold numElements "sum" (SS.sum SS.Infinite)
            , benchWithFold numElements "mean" (SS.mean SS.Infinite)
            , benchWithFold
                numElements
                "welfordMean"
                (SS.welfordMean SS.Infinite)
            ]
        , bgroup
            "scan"
            [ benchWithScan numElements "min" (SS.min SS.Infinite)
            , benchWithScan numElements "max" (SS.max SS.Infinite)
            , benchWithScan numElements "range" (SS.range SS.Infinite)
            , benchWithScan numElements "sum" (SS.sum SS.Infinite)
            , benchWithScan numElements "mean" (SS.mean SS.Infinite)
            , benchWithScan
                numElements
                "welfordMean"
                (SS.welfordMean SS.Infinite)
            ]
        ]
    ]
