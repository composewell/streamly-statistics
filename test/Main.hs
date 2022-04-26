{-# LANGUAGE TupleSections #-}

import Test.Hspec

import Streamly.Statistics

import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Prelude as S

import Prelude hiding (sum, maximum, minimum)

jackKnifeInput :: [Double]
jackKnifeInput = [1.0::Double, 2.0, 3.0, 4.0]

jackMeanRes :: [Double]
jackMeanRes = [3.0, 2.6666666666666665, 2.3333333333333335, 2.0]

main :: IO ()
main = hspec $ do
    describe "Numerical stability while streaming" $ do
        let numElem = 80000
            winSize = 800
            testCaseChunk = [9007199254740992, 1, 1.0 :: Double,
                                9007199254740992, 1, 1, 1, 9007199254740992]
            testCase = take numElem $ cycle testCaseChunk
            deviationLimit = 1
            testFunc f = do
                let c = S.fromList testCase
                a <- runIO $ S.fold (Ring.slidingWindow winSize f) c
                b <- runIO $ S.fold f $ S.drop (numElem - winSize)
                        $ S.map (, Nothing) c
                let c1 = a - b
                it ("should not deviate more than " ++ show deviationLimit)
                    $ c1 >= -1 * deviationLimit && c1 <= deviationLimit

            testFunc2 f = do
                let c = S.fromList [10.0, 11.0, 12.0, 14.0]
                a1 <- runIO $ S.fold (Ring.slidingWindowWith 2 f) c
                a2 <- runIO $ S.fold (Ring.slidingWindowWith 3 f) c
                a3 <- runIO $ S.fold (Ring.slidingWindowWith 4 f) c
                it ("MD should be 1.0 , 1.1111111111111114 , 1.25 but actual is "
                    ++ show a1 ++ " " ++ show a2 ++ " " ++ show a3)
                    (a1 == 1.0 && a2 == 1.1111111111111114 && a3 == 1.25)

        describe "MD" $ testFunc2 md
        describe "Sum" $ testFunc sum
        describe "mean" $ testFunc mean
        describe "welfordMean" $ testFunc welfordMean

    describe "Correctness" $ do
        let winSize = 3
            testCase1 = [31, 41, 59, 26, 53, 58, 97] :: [Double]
            testCase2 = replicate 5 1.0 ++ [7.0]

            testFunc tc f sI sW = do
                let c = S.fromList tc
                a <- runIO $ S.toList $ S.postscan f $ S.map (, Nothing) c
                b <- runIO $ S.toList $ S.postscan
                        (Ring.slidingWindow winSize f) c
                it "Infinite" $ a  == sI
                it ("Finite " ++ show winSize) $ b == sW

            testFuncKurt = do
                let c = S.fromList
                        [ 10.0 :: Double
                        , 11.0
                        , 12.0
                        , 14.0
                        , 10.0
                        , 11.0
                        , 12.0
                        , 14.0
                        ]
                krt <- runIO $ S.fold (Ring.slidingWindow 3 kurtosis) c
                it ( "kurtosis should be 1.5000000000007478 Actual is " ++
                    show krt
                   )
                   (krt == 1.5000000000007478)

            testJackKnife f ls expRes = do
                let arr = Array.fromList ls
                res <- Stream.toList $ f arr
                it ("testJackKnife result should be ="
                    ++ show expRes
                    ++ " Actual is = " ++show res
                    )
                    (res == expRes)

        describe "Kurt" testFuncKurt
        describe "JackKnife Mean" $
            testJackKnife jackKnifeMean jackKnifeInput jackMeanRes

        describe "minimum" $ do
            let scanInf = [31, 31, 31, 26, 26, 26, 26] :: [Double]
                scanWin = [31, 31, 31, 26, 26, 26, 53] :: [Double]
            testFunc testCase1 minimum scanInf scanWin
        describe "maximum" $ do
            let scanInf = [31, 41, 59, 59, 59, 59, 97] :: [Double]
                scanWin = [31, 41, 59, 59, 59, 58, 97] :: [Double]
            testFunc testCase1 maximum scanInf scanWin
        describe "range" $ do
            let scanInf = [0, 10, 28, 33, 33, 33, 71] :: [Double]
                scanWin = [0, 10, 28, 33, 33, 32, 44] :: [Double]
            testFunc testCase1 range scanInf scanWin
        describe "sum" $ do
            let scanInf = [1, 2, 3, 4, 5, 12] :: [Double]
                scanWin = [1, 2, 3, 3, 3, 9] :: [Double]
            testFunc testCase2 sum scanInf scanWin
        describe "mean" $ do
            let scanInf = [1, 1, 1, 1, 1, 2] :: [Double]
                scanWin = [1, 1, 1, 1, 1, 3] :: [Double]
            testFunc testCase2 mean scanInf scanWin
        describe "welfordMean" $ do
            let scanInf = [1, 1, 1, 1, 1, 2] :: [Double]
                scanWin = [1, 1, 1, 1, 1, 3] :: [Double]
            testFunc testCase2 welfordMean scanInf scanWin
