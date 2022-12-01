{-# LANGUAGE TupleSections #-}

import Control.Monad.IO.Class (liftIO)
import Data.Complex (Complex ((:+)))
import Data.Functor.Classes (liftEq2)
import Streamly.Data.Array (Unbox)
import Streamly.Data.Stream (Stream)
import Test.Hspec.Core.Spec (SpecM)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
    (elements, chooseInt, choose, forAll, Property, vectorOf)
import Test.QuickCheck.Monadic (monadicIO, assert)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as V
import qualified Statistics.Sample.Powers as STAT
import qualified Statistics.Transform as STAT
import qualified Streamly.Internal.Data.Array.Mut as MA
import qualified Streamly.Internal.Data.Array.Type as Array
import qualified Streamly.Internal.Data.Fold as Fold
import qualified Streamly.Internal.Data.Ring.Unboxed as Ring
import qualified Streamly.Internal.Data.Stream as Stream
import qualified Streamly.Data.Stream as S

import Prelude hiding (sum, maximum, minimum)

import Streamly.Statistics
import Test.Hspec

tolerance :: Double
tolerance = 0.00001

validate :: Double -> Bool
validate delta  = delta < tolerance

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

testDistributions
    :: (STAT.Powers -> Double)
    -> Fold.Fold IO (Double, Maybe Double) Double
    -> Property
testDistributions func fld =
    forAll (chooseInt (1, 1000)) $ \list_length ->
        forAll (vectorOf list_length (choose (-50.0 :: Double, 100.0)))
            $ \ls ->
                monadicIO $ do
                let var2 = func . STAT.powers 2 $ V.fromList ls
                    strm = S.fromList ls
                var1 <-
                    liftIO $ S.fold (Ring.slidingWindow list_length fld) strm
                assert (validate $ abs (var1 - var2))

testVariance :: Property
testVariance = testDistributions STAT.variance variance

testStdDev :: Property
testStdDev = testDistributions STAT.stdDev stdDev

testFuncMD ::
    Fold.Fold IO ((Double, Maybe Double), IO (MA.Array Double)) Double -> Spec
testFuncMD f = do
                let c = S.fromList [10.0, 11.0, 12.0, 14.0]
                a1 <- runIO $ S.fold (Ring.slidingWindowWith 2 f) c
                a2 <- runIO $ S.fold (Ring.slidingWindowWith 3 f) c
                a3 <- runIO $ S.fold (Ring.slidingWindowWith 4 f) c
                it ("MD should be 1.0 , 1.1111111111111114 , 1.25 but actual is "
                    ++ show a1 ++ " " ++ show a2 ++ " " ++ show a3)
                    (  validate (abs (a1 - 1.0))
                    && validate (abs (a2 - 1.1111111))
                    && validate (abs (a3 - 1.25))
                    )

testFuncKurt :: Spec
testFuncKurt = do
    let c = S.fromList
            [21.3 :: Double, 38.4, 12.7, 41.6]
    krt <- runIO $ S.fold (Ring.slidingWindow 4 kurtosis) c
    it ( "kurtosis should be 1.2762447351370185 Actual is " ++
        show krt
        )

        (validate $ abs (krt - 1.2762447))

testJackKnife :: (Show a, Eq a, Unbox a) =>
       (Array.Array a -> Stream (SpecM ()) a)
    -> [a]
    -> [a]
    -> Spec
testJackKnife f ls expRes = do
    let arr = Array.fromList ls
    res <- Stream.fold Fold.toList $ f arr
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
    freq <- S.fold frequency' $ S.fromList inputList
    it ("Frequency " ++ show freq) $ liftEq2 (==) (==) freq result

testMode :: [Int] -> Maybe (Int, Int) -> Spec
testMode inputList res = do
    mode0 <- S.fold mode $ S.fromList inputList
    it ("Mode " ++ show mode0) $ mode0 == res

testFFT :: Property
testFFT = do
    let lengths = [2, 4, 8, 16]
    forAll (elements lengths) $ \list_length ->
        forAll (vectorOf list_length (choose (-50.0 :: Double, 100.0)))
            $ \ls ->
                monadicIO $ do
                    let tc = map (\x -> x :+ 0) ls
                    let vr = V.toList (STAT.fft (V.fromList tc)
                                        :: V.Vector STAT.CD)
                    marr <- MA.fromList tc
                    fft marr
                    res <- MA.toList marr
                    assert (vr == res)

sampleList :: [Double]
sampleList = [1.0, 2.0, 3.0, 4.0, 5.0]

testResample :: [Double] -> Spec
testResample sample = do
    let sampleArr = Array.fromList sample
        sampleSet = Set.fromList sample
    resampleList <- runIO $ S.fold Fold.toList $ S.unfold resample sampleArr
    let resampleSet = Set.fromList resampleList
        sub = Set.isSubsetOf resampleSet sampleSet
    -- XXX We should not use dynamic output in test description
    it ("resample " ++ show resampleList)
       (Prelude.length resampleList == Array.length sampleArr && sub)

testFoldResamples :: Int -> [Double] -> Spec
testFoldResamples n sample = do
    let arr = Array.fromList sample
    a <- runIO $ S.fold Fold.toList $ foldResamples n arr Fold.mean
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
                        $ fmap (, Nothing) c
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
                a <- runIO $ S.fold Fold.toList $ S.postscan f $ fmap (, Nothing) c
                b <- runIO $ S.fold Fold.toList $ S.postscan
                        (Ring.slidingWindow winSize f) c
                it "Infinite" $ a  == sI
                it ("Finite " ++ show winSize) $ b == sW

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
        describe "MD" $ testFuncMD md
        describe "Kurt" testFuncKurt
        prop "fft" testFFT
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
        prop "variance" testVariance
        prop "stdDev" testStdDev
