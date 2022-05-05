{-# LANGUAGE TupleSections #-}

import Data.Functor.Classes (liftEq2)
import Foreign (Storable)
import Streamly.Internal.Data.Stream.IsStream (SerialT)
import Test.Hspec.Core.Spec (SpecM)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Streamly.Internal.Data.Array.Foreign.Mut as MA
import qualified Streamly.Internal.Data.Array.Foreign.Type as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Internal.Data.Stream.IsStream as Stream
import qualified Streamly.Prelude as S

import Prelude hiding (sum, maximum, minimum)

import Streamly.Statistics
import Test.Hspec

jackKnifeInput :: [Double]
jackKnifeInput = [1.0::Double, 2.0, 3.0, 4.0]

jackMeanRes :: [Double]
jackMeanRes = [3.0, 2.6666666666666665, 2.3333333333333335, 2.0]

jackVarianceRes :: [Double]
jackVarianceRes =
    [ 0.6666666666666661
    , 1.5555555555555554
    , 1.5555555555555545
    , 0.666666666666667
    ]

jackStdDevRes :: [Double]
jackStdDevRes =
    [ 0.8164965809277257
    , 1.247219128924647
    , 1.2472191289246466
    , 0.8164965809277263
    ]

testFuncMD :: (Storable a, Show a, Eq a, Fractional a) =>
    Fold.Fold IO ((a, Maybe a), IO (MA.Array a)) a -> Spec
testFuncMD f = do
                let c = S.fromList [10.0, 11.0, 12.0, 14.0]
                a1 <- runIO $ S.fold (Ring.slidingWindowWith 2 f) c
                a2 <- runIO $ S.fold (Ring.slidingWindowWith 3 f) c
                a3 <- runIO $ S.fold (Ring.slidingWindowWith 4 f) c
                it ("MD should be 1.0 , 1.1111111111111114 , 1.25 but actual is "
                    ++ show a1 ++ " " ++ show a2 ++ " " ++ show a3)
                    (a1 == 1.0 && a2 == 1.1111111111111114 && a3 == 1.25)

testFuncKurt :: Spec
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

testJackKnife :: (Show a, Eq a, Storable a) =>
       (Array.Array a -> SerialT (SpecM ()) a)
    -> [a]
    -> [a]
    -> Spec
testJackKnife f ls expRes = do
    let arr = Array.fromList ls
    res <- Stream.toList $ f arr
    it ("testJackKnife result should be ="
        ++ show expRes
        ++ " Actual is = " ++show res
        )
        (res == expRes)

testFuncHistogram :: Spec
testFuncHistogram = do
    let strm = S.fromList [1..15]
    res <- runIO $
        S.fold (histogram (binOffsetSize (0::Int) (3::Int))) strm
    let expected = Map.fromList
                    [ (0::Int, 2::Int)
                    , (1, 3)
                    , (2, 3)
                    , (3, 3)
                    , (4, 3)
                    , (5, 1)
                    ]

    it ("Map should be = "
        ++ show expected
        ++ " Actual is = "
        ++ show res) (expected == res)

testFuncbinFromSizeN :: Int -> Int -> Int -> Int -> HistBin Int -> SpecWith (Arg Bool)
testFuncbinFromSizeN low binSize nbins x exp0 = do
    let res = binFromSizeN low binSize nbins x
    it ("Bin should be = "
        ++ show exp0
        ++ " Actual is = "
        ++ show res) (res == exp0)

testFuncbinFromToN :: Int -> Int -> Int -> Int -> HistBin Int -> SpecWith ()
testFuncbinFromToN low high n x exp0 = do
    let res = binFromToN low high n x
    it ("Bin should be = "
        ++ show exp0
        ++ " Actual is = "
        ++ show res) (res == exp0)

testFrequency :: [Int] -> Map.Map Int Int -> Spec
testFrequency inputList result = do
    freq <- S.fold frequency $ S.fromList inputList
    it ("Frequency " ++ show freq) $ liftEq2 (==) (==) freq result

testMode :: [Int] -> Maybe (Int, Int) -> Spec
testMode inputList res = do
    mode0 <- S.fold mode $ S.fromList inputList
    it ("Mode " ++ show mode0) $ mode0 == res

sampleList :: [Double]
sampleList = [1.0, 2.0, 3.0, 4.0, 5.0]

testResample :: [Double] -> Spec
testResample sample = do
    let sampleArr = Array.fromList sample
        sampleSet = Set.fromList sample
    resampleList <- runIO $ S.toList $ S.unfold resample sampleArr
    let resampleSet = Set.fromList resampleList
        sub = Set.isSubsetOf resampleSet sampleSet
    -- XXX We should not use dynamic output in test description
    it ("resample " ++ show resampleList)
       (Prelude.length resampleList == Array.length sampleArr && sub)

testFoldResamples :: Int -> [Double] -> Spec
testFoldResamples n sample = do
    let arr = Array.fromList sample
    a <- runIO $ S.toList $ foldResamples n arr Fold.mean
    -- XXX We should not use dynamic output in test description
    it ("foldResamples " ++ show a) (Prelude.length a == n)

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

        describe "MD" $ testFuncMD md
        describe "Kurt" testFuncKurt

        -- Resampling
        describe "JackKnife Mean" $
            testJackKnife jackKnifeMean jackKnifeInput jackMeanRes
        describe "JackKnife Variance" $ do
            testJackKnife jackKnifeVariance jackKnifeInput jackVarianceRes
        describe "JackKnife StdDev" $
            testJackKnife jackKnifeStdDev jackKnifeInput jackStdDevRes

        describe "resample" $ do
            testResample sampleList
        describe "foldResamples 4" $ do
            testFoldResamples 4 sampleList
        describe "foldResamples 6" $ do
            testFoldResamples 6 sampleList

        -- Spread/Mean
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

        -- Probability Distribution
        describe "frequency"
            $ testFrequency
                [1::Int, 1, 2, 3, 3, 3]
                (Map.fromList [(1, 2), (2, 1), (3, 3)])
        describe "Mode" $ testMode [1::Int, 1, 2, 3, 3, 3] (Just (3, 3))
        describe "Mode Empty " $ testMode ([]::[Int]) Nothing

        describe "histogram" testFuncHistogram
        describe "binFromSizeN AboveRange" $
            testFuncbinFromSizeN (0::Int) 2 10 55 AboveRange
        describe "binFromSizeN BelowRange" $
            testFuncbinFromSizeN (0::Int) 2 10 (-1) BelowRange
        describe "binFromSizeN InRange" $
            testFuncbinFromSizeN (0::Int) 2 10 19 (InRange 9)
        describe "binFromSizeN AboveRange" $
            testFuncbinFromSizeN (0::Int) 2 10 20 AboveRange
        describe "binFromToN AboveRange" $
            testFuncbinFromToN (0::Int) 49 10 55 AboveRange
        describe "binFromToN BelowRange" $
            testFuncbinFromToN (0::Int) 49 10 (-1) BelowRange
        describe "binFromToN InRange"    $
            testFuncbinFromToN (0::Int) 49 10 19 (InRange 3)
        describe "binFromToN AboveRange" $
            testFuncbinFromToN (0::Int) 50 10 20 (InRange 4)
