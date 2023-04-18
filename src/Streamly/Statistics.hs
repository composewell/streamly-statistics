-- |
-- Module      : Streamly.Statistics
-- Copyright   : (c) 2020 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- Statistical measures over a stream of data. All operations use numerically
-- stable floating point arithmetic.
--
-- Measurements can be performed over the entire input stream or on a sliding
-- window of fixed or variable size.  Where possible, measures are computed
-- online without buffering the input stream.
--
-- Currently there is no overflow detection.
--
-- References:
--
-- * https://en.wikipedia.org/wiki/Statistics
-- * https://mathworld.wolfram.com/topics/ProbabilityandStatistics.html

-- Resources:
--
-- This may be another useful resource for incremental (non-windowed)
-- computation:
--
-- https://www.researchgate.net/publication/287152055_Incremental_Statistical_Measures
--
-- Sample Statistics
--
-- Terms
--
-- Population: the complete data set from which statistical samples are taken.
--
-- Sample: a subset of the population.
--
-- https://en.wikipedia.org/wiki/Sample_(statistics)
--
-- Estimator:
--
-- Statistical measures can be computed either from the actual population
-- or from samples. Statistical measures computed from the samples provide an
-- estimate of the actual measures of the entire population. Measures computed
-- from samples may not truly reflect the actual measures and may have to be
-- adjusted for biases or errors.
--
-- An "estimator" is a method or function to compute a statistical measure from
-- sampled data. For example, the sample variance is an esitmator of the
-- population variance.
--
-- https://en.wikipedia.org/wiki/Estimator
--
-- Bias:
--
-- The result computed by an estimator may not be centered at the true value as
-- determined by computing the measure for the actual population. Such an
-- estimator is called a biased estimator.  For example, notice how
-- 'sampleVariance' is adjusted for bias.
--
-- https://en.wikipedia.org/wiki/Bias_of_an_estimator
--
-- Consistency:
--
-- https://en.wikipedia.org/wiki/Consistent_estimator

{-# LANGUAGE ScopedTypeVariables #-}
module Streamly.Statistics
    (
    -- * Incremental Folds
    -- | Folds of type @Fold m (a, Maybe a) b@ are incremental sliding window
    -- folds. An input of type @(a, Nothing)@ indicates that the input element
    -- @a@ is being inserted in the window without ejecting an old value
    -- increasing the window size by 1. An input of type @(a, Just a)@
    -- indicates that the first element is being inserted in the window and the
    -- second element is being removed from the window, the window size remains
    -- the same. The window size can only increase and never decrease.
    --
    -- You can compute the statistics over the entire stream using sliding
    -- window folds by keeping the second element of the input tuple as
    -- @Nothing@.
    --
      Window.lmap
    , Window.cumulative

    -- * Summary Statistics
    -- | See https://en.wikipedia.org/wiki/Summary_statistics .

    -- ** Sums
    , Window.length
    , Window.sum
    , Window.sumInt
    , Window.powerSum

    -- ** Location
    -- | See https://en.wikipedia.org/wiki/Location_parameter .
    --
    -- See https://en.wikipedia.org/wiki/Central_tendency .
    , minimum
    , maximum
    , rawMoment
    , rawMomentFrac

    -- Pythagorean means (https://en.wikipedia.org/wiki/Pythagorean_means)
    , mean
    , welfordMean
    , geometricMean
    , harmonicMean

    , quadraticMean

    -- Generalized mean
    , powerMean
    , powerMeanFrac

    -- ** Weighted Means
    -- | Exponential Smoothing.
    , ewma
    , ewmaAfterMean
    , ewmaRampUpSmoothing

    -- ** Spread
    -- | Second order central moment is a statistical measure of dispersion.
    -- The \(k\)th moment about the mean (or \(k\)th central moment) is defined
    -- as:
    --
    -- \(\mu_k = \frac{1}{n}\sum_{i=1}^n {(x_{i}-\mu)}^k\)
    --
    -- See https://mathworld.wolfram.com/CentralMoment.html .
    --
    -- See https://en.wikipedia.org/wiki/Statistical_dispersion .
    , range
    , md
    , variance
    , stdDev

    -- ** Shape
    -- | Third and fourth order central moments are a measure of shape.
    --
    -- See https://en.wikipedia.org/wiki/Shape_parameter .
    --
    -- See https://en.wikipedia.org/wiki/Standardized_moment .
    , skewness
    , kurtosis

    -- XXX Move to Statistics.Sample or Statistics.Estimation module?
    -- ** Estimation
    , sampleVariance
    , sampleStdDev
    , stdErrMean

    -- ** Resampling
    , resample
    , foldResamples
    , jackKnifeMean
    , jackKnifeVariance
    , jackKnifeStdDev

    -- ** Probability Distribution
    , frequency
    , frequency'
    , mode

    -- Histograms
    , HistBin (..)
    , binOffsetSize
    , binFromSizeN
    , binFromToN
    , binBoundaries
    , histogram

    -- * Transforms
    , fft
    )
where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Bits (Bits(complement, shiftL, shiftR, (.&.), (.|.)))
import Data.Complex (Complex ((:+)))
import Data.Function ((&))
import Data.Functor.Identity (runIdentity, Identity)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Streamly.Data.Array (Array, length, Unbox)
import Streamly.Data.Fold (Tee(..))
import Streamly.Data.Stream (Stream)
import Streamly.Internal.Data.Array.Type (unsafeIndexIO)
import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..))
import Streamly.Internal.Data.Stream.StreamD.Step (Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))
import Streamly.Internal.Data.Unfold.Type (Unfold(..))
import System.Random.MWC (createSystemRandom, uniformRM)

import qualified Data.Map.Strict as Map
import qualified Deque.Strict as Deque
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Data.Array as Array hiding (read)
import qualified Streamly.Internal.Data.Array as Array (read)
import qualified Streamly.Data.MutArray as MA
import qualified Streamly.Internal.Data.Array.Mut as MA
    (getIndexUnsafe, putIndexUnsafe, unsafeSwapIndices)
import qualified Streamly.Internal.Data.Fold.Window as Window
import qualified Streamly.Data.Stream as Stream

import Prelude hiding (length, sum, minimum, maximum)

-- TODO: Overflow checks. Would be good if we can directly replace the
-- operations with overflow checked operations.
--
-- See https://hackage.haskell.org/package/safe-numeric
-- See https://hackage.haskell.org/package/safeint
--
-- TODO We have many of these functions in Streamly.Data.Fold as well. Need to
-- think about deduplication.

-------------------------------------------------------------------------------
-- Transforms
-------------------------------------------------------------------------------

-- XXX These utility functions can be moved to streamly-numeric

-- | Test if the given integer value is a power of 2.
{-# INLINE isPower2 #-}
isPower2 :: Int -> Bool
isPower2 n = n .&. (n - 1) == 0

-- | Create a power of 2
--
-- Argument must be less than 64 assuming 64-bit Int size.
--
{-# INLINE _power2 #-}
_power2 :: Int -> Int
_power2 n = shiftL 1 n

-- | Create a bit mask with lower n bits 0 and the rest as 1.
--
-- Argument must be less than 64 assuming 64-bit Int size.
--
{-# INLINE maskLowerN #-}
maskLowerN :: Int -> Int
maskLowerN n = complement (shiftL 1 n - 1)

-- | Compute the base 2 logarithm of the given value.
--
-- Assumes the Int size to be 64-bit.
--
{-# INLINE logBase2 #-}
logBase2 :: Int -> Int
logBase2 v0
    | v0 <= 0   = error $ "logBase2: input must be greater than 0 " ++ show v0
    | otherwise = go 32 0 v0

    where

    go !bits !result !v
        | bits == 0 = result
        | v .&. maskLowerN bits /= 0 =
             go (bits `shiftR` 1) (result .|. bits) (v `shiftR` bits)
        | otherwise = go (bits `shiftR` 1) result v

-- Algo translated from the statistics library.
--
-- XXX We can use a wrapper API that takes an array of Double input instead of
-- array of Complex.
--
-- | Compute fast fourier transform of an array of 'Complex' values.
--
-- Array length must be power of 2.
--
{-# INLINE fft #-}
fft :: MonadIO m => MA.MutArray (Complex Double) -> m ()
fft marr
    | isPower2 len = bitReverse 0 0
    | otherwise  = error "fft: Array length must be power of 2"

    where

    len = MA.length marr

    halve x = x `shiftR` 1

    twice x = x `shiftL` 1

    inner i j k
        | k <= j  = inner i (j - k) (halve k)
        | otherwise = bitReverse (i + 1) (j + k)

    bitReverse i j
        | i == len - 1 = stage 0 1
        | otherwise = do
            when (i < j) $ MA.unsafeSwapIndices i j marr
            inner i j (halve len)

    log2len = logBase2 len

    stage l !l1
        | l == log2len = return ()
        | otherwise = do
            let !l2 = twice l1
                !e  = -6.283185307179586/fromIntegral l2
                flight j !a | j == l1   = stage (l + 1) l2
                            | otherwise = do
                    let butterfly i | i >= len  = flight (j + 1) (a + e)
                                    | otherwise = do
                            let i1 = i + l1
                            xi1 :+ yi1 <- MA.getIndexUnsafe i1 marr
                            let !c = cos a
                                !s = sin a
                                d  = (c * xi1 - s * yi1) :+ (s * xi1 + c * yi1)
                            ci <- MA.getIndexUnsafe i marr
                            MA.putIndexUnsafe i1 marr (ci - d)
                            MA.putIndexUnsafe i marr (ci + d)
                            butterfly (i + l2)
                    butterfly j
            flight 0 0

-------------------------------------------------------------------------------
-- Location
-------------------------------------------------------------------------------

-- Theoretically, we can approximate minimum in a rolling window by using a
-- 'powerMean' with sufficiently large negative power.
--
-- XXX If we need to know the minimum in the window only once in a while then
-- we can use linear search when it is extracted and not pay the cost all the
-- time.
--
-- | The minimum element in a rolling window.
--
-- For smaller window sizes (< 30) Streamly.Data.Fold.Window.minimum performs
-- better.  If you want to compute the minimum of the entire stream Fold.min
-- from streamly package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE minimum #-}
minimum :: (Monad m, Ord a) => Fold m (a, Maybe a) a
minimum = Fold step initial extract

    where

    initial =
        return
            $ Partial
            $ Tuple3' (0 :: Int) (0 :: Int) (mempty :: Deque.Deque (Int, a))

    step (Tuple3' i w q) (a, ma) =
        case ma of
            Nothing ->
                return
                    $ Partial
                    $ Tuple3'
                        (i + 1)
                        (w + 1)
                        (headCheck i q (w + 1) & dqloop (i, a))
            Just _ ->
                return
                    $ Partial
                    $ Tuple3' (i + 1) w (headCheck i q w & dqloop (i,a))

    {-# INLINE headCheck #-}
    headCheck i q w =
        case Deque.uncons q of
            Nothing -> q
            Just (ia', q') ->
                if fst ia' <= i - w
                then q'
                else q

    dqloop ia q =
        case Deque.unsnoc q of
            Nothing -> Deque.snoc ia q
            -- XXX This can be improved for the case of `=`
            Just (ia', q') ->
                if snd ia <= snd ia'
                then dqloop ia q'
                else Deque.snoc ia q

    extract (Tuple3' _ _ q) =
        return
            $ snd
            $ fromMaybe (0, error "minimum: Empty stream")
            $ Deque.head q

-- Theoretically, we can approximate maximum in a rolling window by using a
-- 'powerMean' with sufficiently large positive power.
--
-- | The maximum element in a rolling window.
--
-- For smaller window sizes (< 30) Streamly.Data.Fold.Window.maximum performs
-- better.  If you want to compute the maximum of the entire stream
-- Streamly.Data.Fold.maximum from streamly package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE maximum #-}
maximum :: (Monad m, Ord a) => Fold m (a, Maybe a) a
maximum = Fold step initial extract

    where

    initial =
        return
            $ Partial
            $ Tuple3' (0 :: Int) (0 :: Int) (mempty :: Deque.Deque (Int, a))

    step (Tuple3' i w q) (a, ma) =
        case ma of
            Nothing ->
                return
                    $ Partial
                    $ Tuple3'
                        (i + 1)
                        (w + 1)
                        (headCheck i q (w + 1) & dqloop (i, a))
            Just _ ->
                return
                    $ Partial
                    $ Tuple3' (i + 1) w (headCheck i q w & dqloop (i,a))

    {-# INLINE headCheck #-}
    headCheck i q w =
        case Deque.uncons q of
            Nothing -> q
            Just (ia', q') ->
                if fst ia' <= i - w
                then q'
                else q

    dqloop ia q =
        case Deque.unsnoc q of
            Nothing -> Deque.snoc ia q
            -- XXX This can be improved for the case of `=`
            Just (ia', q') ->
                if snd ia >= snd ia'
                then dqloop ia q'
                else Deque.snoc ia q

    extract (Tuple3' _ _ q) =
        return
            $ snd
            $ fromMaybe (0, error "maximum: Empty stream")
            $ Deque.head q

-------------------------------------------------------------------------------
-- Mean
-------------------------------------------------------------------------------

-- | Arithmetic mean of elements in a sliding window:
--
-- \(\mu = \frac{\sum_{i=1}^n x_{i}}{n}\)
--
-- This is also known as the Simple Moving Average (SMA) when used in the
-- sliding window and Cumulative Moving Avergae (CMA) when used on the entire
-- stream.
--
-- Mean is the same as the first raw moment.
--
-- \(\mu = \mu'_1\)
--
-- >>> mean = rawMoment 1
-- >>> mean = powerMean 1
-- >>> mean = Fold.teeWith (/) sum length
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE mean #-}
mean :: forall m a. (Monad m, Fractional a) => Fold m (a, Maybe a) a
mean = Window.mean

-- | Recompute mean from old mean when an item is removed from the sample.
{-# INLINE _meanSubtract #-}
_meanSubtract :: Fractional a => Int -> a -> a -> a
_meanSubtract n oldMean oldItem =
    let delta = (oldItem - oldMean) / fromIntegral (n - 1)
     in oldMean - delta

-- | Recompute mean from old mean when an item is added to the sample.
{-# INLINE meanAdd #-}
meanAdd :: Fractional a => Int -> a -> a -> a
meanAdd n oldMean newItem =
    let delta = (newItem - oldMean) / fromIntegral (n + 1)
     in oldMean + delta

-- We do not carry rounding errors, therefore, this would be less numerically
-- stable than the kbn mean.
--
-- | Recompute mean from old mean when an item in the sample is replaced.
{-# INLINE meanReplace #-}
meanReplace :: Fractional a => Int -> a -> a -> a -> a
meanReplace n oldMean oldItem newItem =
    let n1 = fromIntegral n
        -- Compute two deltas instead of a single (newItem - oldItem) because
        -- the latter would be too small causing rounding errors.
        delta1 = (newItem - oldMean) / n1
        delta2 = (oldItem - oldMean) / n1
     in (oldMean + delta1) - delta2

-- | Same as 'mean' but uses Welford's algorithm to compute the mean
-- incrementally.
--
-- It maintains a running mean instead of a running sum and adjusts the mean
-- based on a new value.  This is slower than 'mean' because of using the
-- division operation on each step and it is numerically unstable (as of now).
-- The advantage over 'mean' could be no overflow if the numbers are large,
-- because we do not maintain a sum, but that is a highly unlikely corner case.
--
-- /Internal/
{-# INLINE welfordMean #-}
welfordMean :: forall m a. (Monad m, Fractional a) => Fold m (a, Maybe a) a
welfordMean = Fold step initial extract

    where

    initial =
        return
            $ Partial
            $ Tuple'
                (0 :: a)   -- running mean
                (0 :: Int) -- count of items in the window

    step (Tuple' oldMean w) (new, mOld) =
        return
            $ Partial
            $ case mOld of
                Nothing -> Tuple' (meanAdd w oldMean new) (w + 1)
                Just old -> Tuple' (meanReplace w oldMean old new) w

    extract (Tuple' x _) = return x

-------------------------------------------------------------------------------
-- Moments
-------------------------------------------------------------------------------

-- XXX We may have chances of overflow if the powers are high or the numbers
-- are large. A limited mitigation could be to use welford style avg
-- computation. Do we need an overflow detection?
--
-- | Raw moment is the moment about 0. The \(k\)th raw moment is defined as:
--
-- \(\mu'_k = \frac{\sum_{i=1}^n x_{i}^k}{n}\)
--
-- >>> rawMoment k = Fold.teeWith (/) (powerSum p) length
--
-- See https://en.wikipedia.org/wiki/Moment_(mathematics) .
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE rawMoment #-}
rawMoment :: (Monad m, Fractional a) => Int -> Fold m (a, Maybe a) a
rawMoment k = Fold.teeWith (/) (Window.powerSum k) Window.length

-- | Like 'rawMoment' but powers can be negative or fractional. This is
-- slower than 'rawMoment' for positive intergal powers.
--
-- >>> rawMomentFrac p = Fold.teeWith (/) (powerSumFrac p) length
--
{-# INLINE rawMomentFrac #-}
rawMomentFrac :: (Monad m, Floating a) => a -> Fold m (a, Maybe a) a
rawMomentFrac k = Fold.teeWith (/) (Window.powerSumFrac k) Window.length

-- XXX Overflow can happen when large powers or large numbers are used. We can
-- keep a running mean instead of running sum but that won't mitigate the
-- overflow possibility by much. The overflow can still happen when computing
-- the mean incrementally.

-- | The \(k\)th power mean of numbers \(x_1, x_2, \ldots, x_n\) is:
--
-- \(M_k = \left( \frac{1}{n} \sum_{i=1}^n x_i^k \right)^{\frac{1}{k}}\)
--
-- \(powerMean(k) = (rawMoment(k))^\frac{1}{k}\)
--
-- >>> powerMean k = (** (1 / fromIntegral k)) <$> rawMoment k
--
-- All other means can be expressed in terms of power mean. It is also known as
-- the generalized mean.
--
-- See https://en.wikipedia.org/wiki/Generalized_mean
--
{-# INLINE powerMean #-}
powerMean :: (Monad m, Floating a) => Int -> Fold m (a, Maybe a) a
powerMean k = (** (1 / fromIntegral k)) <$> rawMoment k

-- | Like 'powerMean' but powers can be negative or fractional. This is
-- slower than 'powerMean' for positive intergal powers.
--
-- >>> powerMeanFrac k = (** (1 / k)) <$> rawMomentFrac k
--
{-# INLINE powerMeanFrac #-}
powerMeanFrac :: (Monad m, Floating a) => a -> Fold m (a, Maybe a) a
powerMeanFrac k = (** (1 / k)) <$> rawMomentFrac k

-- | The harmonic mean of the positive numbers \(x_1, x_2, \ldots, x_n\) is
-- defined as:
--
-- \(HM = \frac{n}{\frac1{x_1} + \frac1{x_2} + \cdots + \frac1{x_n}}\)
--
-- \(HM = \left(\frac{\sum\limits_{i=1}^n x_i^{-1}}{n}\right)^{-1}\)
--
-- >>> harmonicMean = Fold.teeWith (/) length (lmap recip sum)
-- >>> harmonicMean = powerMeanFrac (-1)
--
-- See https://en.wikipedia.org/wiki/Harmonic_mean .
--
{-# INLINE harmonicMean #-}
harmonicMean :: (Monad m, Fractional a) => Fold m (a, Maybe a) a
harmonicMean = Fold.teeWith (/) Window.length (Window.lmap recip Window.sum)

-- | Geometric mean, defined as:
--
-- \(GM = \sqrt[n]{x_1 x_2 \cdots x_n}\)
--
-- \(GM = \left(\prod_{i=1}^n x_i\right)^\frac{1}{n}\)
--
-- or, equivalently, as the arithmetic mean in log space:
--
-- \(GM = e ^{{\frac{\sum_{i=1}^{n}\ln a_i}{n}}}\)
--
-- >>> geometricMean = exp <$> lmap log mean
--
-- See https://en.wikipedia.org/wiki/Geometric_mean .
{-# INLINE geometricMean #-}
geometricMean :: (Monad m, Floating a) => Fold m (a, Maybe a) a
geometricMean = exp <$> Window.lmap log mean

-- | The quadratic mean or root mean square (rms) of the numbers
-- \(x_1, x_2, \ldots, x_n\) is defined as:
--
-- \(RMS = \sqrt{ \frac{1}{n} \left( x_1^2 + x_2^2 + \cdots + x_n^2 \right) }.\)
--
-- >>> quadraticMean = powerMean 2
--
-- See https://en.wikipedia.org/wiki/Root_mean_square .
--
{-# INLINE quadraticMean #-}
quadraticMean :: (Monad m, Floating a) => Fold m (a, Maybe a) a
quadraticMean = powerMean 2

-------------------------------------------------------------------------------
-- Weighted Means
-------------------------------------------------------------------------------

-- XXX Is this numerically stable? We can use the kbn summation here.
-- | ewmaStep smoothing-factor old-value new-value
{-# INLINE ewmaStep #-}
ewmaStep :: Double -> Double -> Double -> Double
ewmaStep k x0 x1 = (1 - k) * x0 + k * x1

-- XXX Compute this in a sliding window?
--
-- | @ewma smoothingFactor@.
--
-- @ewma@ of an empty stream is 0.
--
-- Exponential weighted moving average, \(s_n\), of \(n\) values,
-- \(x_1,\ldots,x_n\), is defined recursively as:
--
-- \(\begin{align} s_0& = x_0\\ s_n & = \alpha x_{n} + (1-\alpha)s_{n-1},\quad n>0 \end{align}\)
--
-- If we expand the recursive term it becomes an exponential series:
--
-- \(s_n = \alpha \left[x_n + (1-\alpha)x_{n-1} + (1-\alpha)^2 x_{n-2} + \cdots + (1-\alpha)^{n-1} x_1 \right] + (1-\alpha)^n x_0\)
--
-- where \(\alpha\), the smoothing factor, is in the range \(0 <\alpha < 1\).
-- More the value of \(\alpha\), the more weight is given to newer values.  As
-- a special case if it is 0 then the weighted sum would always be the same as
-- the oldest value, if it is 1 then the sum would always be the same as the
-- newest value.
--
-- See https://en.wikipedia.org/wiki/Moving_average
--
-- See https://en.wikipedia.org/wiki/Exponential_smoothing
--
{-# INLINE ewma #-}
ewma :: Monad m => Double -> Fold m Double Double
ewma k = extract <$> Fold.foldl' step (Tuple' 0 1)

    where

    step (Tuple' x0 k1) x = Tuple' (ewmaStep k1 x0 x) k

    extract (Tuple' x _) = x

-- XXX It can perhaps perform better if implemented as a custom fold?
--
-- | @ewma n k@ is like 'ewma' but uses the mean of the first @n@ values and
-- then uses that as the initial value for the @ewma@ of the rest of the
-- values.
--
-- This can be used to reduce the effect of volatility of the initial value
-- when k is too small.
--
{-# INLINE ewmaAfterMean #-}
ewmaAfterMean :: Monad m => Int -> Double -> Fold m Double Double
ewmaAfterMean n k =
    Fold.concatMap (\i -> (Fold.foldl' (ewmaStep k) i)) (Fold.take n Fold.mean)

-- | @ewma n k@ is like 'ewma' but uses 1 as the initial smoothing factor and
-- then exponentially smooths it to @k@ using @n@ as the smoothing factor.
--
-- This is significantly faster than 'ewmaAfterMean'.
--
{-# INLINE ewmaRampUpSmoothing #-}
ewmaRampUpSmoothing :: Monad m => Double -> Double -> Fold m Double Double
ewmaRampUpSmoothing n k1 = extract <$> Fold.foldl' step initial

    where

    initial = Tuple' 0 1

    step (Tuple' x0 k0) x1 =
        let x = ewmaStep k0 x0 x1
            k = ewmaStep n k0 k1
        in Tuple' x k

    extract (Tuple' x _) = x

-------------------------------------------------------------------------------
-- Spread/Dispersion
-------------------------------------------------------------------------------

-- | The difference between the maximum and minimum elements of a rolling window.
--
-- >>> range = Fold.teeWith (-) maximum minimum
--
-- If you want to compute the range of the entire stream @Fold.teeWith (-)
-- Fold.maximum Fold.minimum@ from the streamly package would be much faster.
--
-- /Space/: \(\mathcal{O}(n)\) where @n@ is the window size.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE range #-}
range :: (Monad m, Num a, Ord a) => Fold m (a, Maybe a) a
range = Fold.teeWith (-) maximum minimum

-- | @md n@ computes the mean absolute deviation (or mean deviation) in a
-- sliding window of last @n@ elements in the stream.
--
-- The mean absolute deviation of the numbers \(x_1, x_2, \ldots, x_n\) is:
--
-- \(MD = \frac{1}{n}\sum_{i=1}^n |x_i-\mu|\)
--
-- Note: It is expensive to compute MD in a sliding window. We need to
-- maintain a ring buffer of last n elements and maintain a running mean, when
-- the result is extracted we need to compute the difference of all elements
-- from the mean and get the average. Using standard deviation may be
-- computationally cheaper.
--
-- See https://en.wikipedia.org/wiki/Average_absolute_deviation .
--
-- /Pre-release/
{-# INLINE md #-}
md ::  MonadIO m => Fold m ((Double, Maybe Double), m (MA.MutArray Double)) Double
md =
    Fold.rmapM computeMD
        $ Fold.tee (Fold.lmap fst mean) (Fold.lmap snd Fold.latest)

    where

    computeMD (mn, rng) =
        case rng of
            Just action -> do
                arr <- action
                Stream.fold Fold.mean
                    $ fmap (\a -> abs (mn - a))
                    $ Stream.unfold MA.reader arr
            Nothing -> return 0.0

-- | The variance \(\sigma^2\) of a population of \(n\) equally likely values
-- is defined as the average of the squares of deviations from the mean
-- \(\mu\). In other words, second moment about the mean:
--
-- \(\sigma^2 = \frac{1}{n}\sum_{i=1}^n {(x_{i}-\mu)}^2\)
--
-- \(\sigma^2 = rawMoment(2) - \mu^2\)
--
-- \(\mu_2 = -(\mu'_1)^2 + \mu'_2\)
--
-- Note that the variance would be biased if applied to estimate the population
-- variance from a sample of the population. See 'sampleVariance'.
--
-- See https://en.wikipedia.org/wiki/Variance.
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE variance #-}
variance :: (Monad m, Fractional a) => Fold m (a, Maybe a) a
variance = Fold.teeWith (\p2 m -> p2 - m ^ (2 :: Int)) (rawMoment 2) mean

-- | Standard deviation \(\sigma\) is the square root of 'variance'.
--
-- This is the population standard deviation or uncorrected sample standard
-- deviation.
--
-- >>> stdDev = sqrt <$> variance
--
-- See https://en.wikipedia.org/wiki/Standard_deviation .
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE stdDev #-}
stdDev :: (Monad m, Floating a) => Fold m (a, Maybe a) a
stdDev = sqrt <$> variance

-- | Skewness \(\gamma\) is the standardized third central moment defined as:
--
-- \(\tilde{\mu}_3 = \frac{\mu_3}{\sigma^3}\)
--
-- The third central moment can be computed in terms of raw moments:
--
-- \(\mu_3 = 2(\mu'_1)^3 - 3\mu'_1\mu'_2 + \mu'_3\)
--
-- Substituting \(\mu'_1 = \mu\), and \(\mu'_2 = \mu^2 + \sigma^2\):
--
-- \(\mu_3 = -\mu^3 - 3\mu\sigma^2 + \mu'_3\)
--
-- Skewness is a measure of symmetry of the probability distribution. It is 0
-- for a symmetric distribution, negative for a distribution that is skewed
-- towards left, positive for a distribution skewed towards right.
--
-- For a normal like distribution the median can be found around
-- \(\mu - \frac{\gamma\sigma}{6}\) and the mode can be found around
-- \(\mu - \frac{\gamma \sigma}{2}\).
--
-- See https://en.wikipedia.org/wiki/Skewness .
--
{-# INLINE skewness #-}
skewness :: (Monad m, Floating a) => Fold m (a, Maybe a) a
skewness =
    unTee
        $ (\rm3 sd mu ->
            rm3 / sd ^ (3 :: Int) - 3 * (mu / sd) - (mu / sd) ^ (3 :: Int)
          )
        <$> Tee (rawMoment 3)
        <*> Tee stdDev
        <*> Tee mean

-- XXX We can compute the 2nd, 3rd, 4th raw moments by repeatedly multiplying
-- instead of computing the powers every time.
--
-- | Kurtosis \(\kappa\) is the standardized fourth central moment, defined as:
--
-- \(\tilde{\mu}_4 = \frac{\mu_4}{\sigma^4}\)
--
-- The fourth central moment can be computed in terms of raw moments:
--
-- \(\mu_4 = -3(\mu'_1)^4 + 6(\mu'_1)^2\mu'_2 - 4\mu'_1\mu'_3\ + \mu'_4\)
--
-- Substituting \(\mu'_1 = \mu\), and \(\mu'_2 = \mu^2 + \sigma^2\):
--
-- \(\mu_4 = 3\mu^4 + 6\mu^2\sigma^2 - 4\mu\mu'_3 + \mu'_4\)
--
-- It is always non-negative. It is 0 for a point distribution, low for light
-- tailed (platykurtic) distributions and high for heavy tailed (leptokurtic)
-- distributions.
--
-- \(\kappa >= \gamma^2 + 1\)
--
-- For a normal distribution \(\kappa = 3\sigma^4\).
--
-- See https://en.wikipedia.org/wiki/Kurtosis .
--
{-# INLINE kurtosis #-}
kurtosis :: (Monad m, Floating a) => Fold m (a, Maybe a) a
kurtosis =
    unTee
        $ (\rm4 rm3 sd mu ->
             ( 3 * mu ^ (4 :: Int)
            + 6 * mu ^ (2 :: Int) * sd ^ (2 :: Int)
            - 4 * mu * rm3
            + rm4) / (sd ^ (4 :: Int))
          )
        <$> Tee (rawMoment 4)
        <*> Tee (rawMoment 3)
        <*> Tee stdDev
        <*> Tee mean

-------------------------------------------------------------------------------
-- Estimation
-------------------------------------------------------------------------------

-- | Unbiased sample variance i.e. the variance of a sample corrected to
-- better estimate the variance of the population, defined as:
--
-- \(s^2 = \frac{1}{n - 1}\sum_{i=1}^n {(x_{i}-\mu)}^2\)
--
-- \(s^2 = \frac{n}{n - 1} \times \sigma^2\).
--
-- See https://en.wikipedia.org/wiki/Bessel%27s_correction.
--
{-# INLINE sampleVariance #-}
sampleVariance :: (Monad m, Fractional a) => Fold m (a, Maybe a) a
sampleVariance = Fold.teeWith (\n s2 -> n * s2 / (n - 1)) Window.length variance

-- | Sample standard deviation:
--
-- \(s = \sqrt{sampleVariance}\)
--
-- >>> sampleStdDev = sqrt <$> sampleVariance
--
-- See https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
-- .
--
{-# INLINE sampleStdDev #-}
sampleStdDev :: (Monad m, Floating a) => Fold m (a, Maybe a) a
sampleStdDev = sqrt <$> sampleVariance

-- | Standard error of the sample mean (SEM), defined as:
--
-- \( SEM = \frac{sampleStdDev}{\sqrt{n}} \)
--
-- See https://en.wikipedia.org/wiki/Standard_error .
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE stdErrMean #-}
stdErrMean :: (Monad m, Floating a) => Fold m (a, Maybe a) a
stdErrMean = Fold.teeWith (\sd n -> sd / sqrt n) sampleStdDev Window.length

-------------------------------------------------------------------------------
-- Resampling
-------------------------------------------------------------------------------

{-# INLINE foldArray #-}
foldArray :: Unbox a => Fold Identity a b -> Array a -> b
foldArray f = runIdentity . Stream.fold f . Array.read

-- XXX Is this numerically stable? Should we keep the rounding error in the sum
-- and take it into account when subtracting?
--
-- | Given an array of @n@ items, compute mean of @(n - 1)@ items at a time,
-- producing a stream of all possible mean values omitting a different item
-- every time.
--
{-# INLINE jackKnifeMean #-}
jackKnifeMean :: (Monad m, Fractional a, Unbox a) => Array a -> Stream m a
jackKnifeMean arr = do
    let len = fromIntegral (length arr - 1)
        s = foldArray Fold.sum arr
     in fmap (\b -> (s - b) / len) $ Array.read arr

-- | Given an array of @n@ items, compute variance of @(n - 1)@ items at a time,
-- producing a stream of all possible variance values omitting a different item
-- every time.
--
{-# INLINE jackKnifeVariance #-}
jackKnifeVariance :: (Monad m, Fractional a, Unbox a) =>
    Array a -> Stream m a
jackKnifeVariance arr = do
    let len = fromIntegral $ length arr - 1
        foldSums (s, s2) x = (s + x, s2 + x ^ (2 :: Int))
        (sum, sum2) = foldArray (Fold.foldl' foldSums (0.0, 0.0)) arr
        var x = (sum2 - x ^ (2 :: Int)) / len -  ((sum - x) / len) ^ (2::Int)
     in fmap var $ Array.read arr

-- | Standard deviation computed from 'jackKnifeVariance'.
--
{-# INLINE jackKnifeStdDev #-}
jackKnifeStdDev :: (Monad m, Unbox a, Floating a) =>
    Array a -> Stream m a
jackKnifeStdDev = fmap sqrt . jackKnifeVariance

-- XXX This can be made more modular if the replicateM unfold can take count
-- from the seed.
--
-- | Randomly select elements from an array, with replacement, producing
-- a stream of the same size as the original array.
{-# INLINE resample #-}
resample :: (MonadIO m, Unbox a) => Unfold m (Array a) a
resample = Unfold step inject

    where

    inject arr = liftIO $ do
        g <- createSystemRandom
        return $ (g, arr, length arr, 0)

    chooseOne g arr len = do
        i <- uniformRM (0, len - 1) g
        unsafeIndexIO i arr

    step (g, arr, len, idx) = liftIO $ do
        if idx >= len
        then return Stop
        else do
            e <- chooseOne g arr len
            return $ Yield e (g, arr, len, idx + 1)

-- XXX Use concurrent combinators

-- | Resample an array multiple times and run the supplied fold on each
-- resampled stream, producing a stream of fold results. The fold is usually an
-- estimator fold.
{-# INLINE foldResamples #-}
foldResamples :: (MonadIO m, Unbox a) =>
       Int          -- ^ Number of resamples to compute.
    -> Array a      -- ^ Original sample.
    -> Fold m a b   -- ^ Estimator fold
    -> Stream m b
foldResamples n arr fld =
    Stream.sequence
        $ Stream.replicate n (Stream.fold fld $ Stream.unfold resample arr)

-------------------------------------------------------------------------------
-- Probability Distribution
-------------------------------------------------------------------------------

-- XXX We can use a Windowed classifyWith operation, that will allow us to
-- express windowed frequency, mode, histograms etc idiomatically.

-- | Count the frequency of elements in a sliding window.
--
-- >>> input = Stream.fromList [1,1,3,4,4::Int]
-- >>> f = Ring.slidingWindow 4 Statistics.frequency
-- >>> Stream.fold f input
-- fromList [(1,1),(3,1),(4,2)]
--
{-# INLINE frequency #-}
frequency :: (Monad m, Ord a) => Fold m (a, Maybe a) (Map a Int)
frequency = Fold.foldl' step Map.empty

    where

    decrement v =
        if v == 1
        then Nothing
        else Just (v - 1)

    step refCountMap (new, mOld) =
        let m1 = Map.insertWith (+) new 1 refCountMap
        in case mOld of
                Just k -> Map.update decrement k m1
                Nothing -> m1

-- XXX Check if the performance of window frequency is the same as this in the
-- full case, if so remove this.
-- XXX This is available in the streamly package as well.

-- | Determine the frequency of each element in the stream.
--
{-# INLINE frequency' #-}
frequency' :: (Monad m, Ord a) => Fold m a (Map a Int)
frequency' = Fold.toMap id Fold.length

-- | Find out the most frequently ocurring element in the stream and its
-- frequency.
--
{-# INLINE mode #-}
mode :: (Monad m, Ord a) => Fold m a (Maybe (a, Int))
mode = Fold.rmapM findMax frequency'

    where

    fmax k v Nothing = Just (k, v)
    fmax k v old@(Just (_, v1))
        | v > v1 = Just (k, v)
        | otherwise = old

    findMax = return . Map.foldrWithKey fmax Nothing

-------------------------------------------------------------------------------
-- Histograms
-------------------------------------------------------------------------------

-- | @binOffsetSize offset binSize input@. Given an integral input value,
-- return its bin index provided that each bin contains @binSize@ items and the
-- bins are aligned such that the 0 index bin starts at @offset@ from 0. If
-- offset = 0 then the bin with index 0 would have values from 0 to binSize -
-- 1.
--
-- This API does not put a bound on the number of bins, therefore, the number
-- of bins could be potentially large depending on the range of values.
--
{-# INLINE binOffsetSize #-}
binOffsetSize :: Integral a => a -> a -> a -> a
binOffsetSize offset binSize x = (x - offset) `div` binSize

data HistBin a = BelowRange | InRange a | AboveRange deriving (Eq, Show)

instance (Eq a, Ord a) => Ord (HistBin a) where
    compare BelowRange BelowRange = EQ
    compare BelowRange (InRange _) = LT
    compare BelowRange AboveRange = LT

    compare (InRange _) BelowRange = GT
    compare (InRange x) (InRange y)= x `compare` y
    compare (InRange _) AboveRange = LT

    compare AboveRange BelowRange = GT
    compare AboveRange (InRange _) = GT
    compare AboveRange AboveRange = EQ

-- | @binFromSizeN low binSize nbins input@. Classify @input@ into bins
-- specified by a @low@ limit, @binSize@ and @nbins@. Inputs below the lower
-- limit are classified into 'BelowRange' and inputs above the highest bin are
-- classified into 'AboveRange'. 'InRange' inputs are classified into bins
-- starting from bin index 0.
--
{-# INLINE binFromSizeN #-}
binFromSizeN :: Integral a => a -> a -> a -> a -> HistBin a
binFromSizeN low binSize nbins x =
    let high = low + binSize * nbins
     in if x < low
        then BelowRange
        else if x >= high
             then AboveRange
             else InRange ((x - low) `div` binSize)

-- | @binFromToN low high nbins input@. Like @binFromSizeN@ except that a range
-- of lower and higher limit is specified. @binSize@ is computed using the
-- range and @nbins@. @nbins@ is rounded to the range @0 < nbins < (high - low
-- + 1)@. @high >= low@ must hold.
--
{-# INLINE binFromToN #-}
binFromToN :: Integral a => a -> a -> a -> a -> HistBin a
binFromToN low high n x =
    let count = high - low + 1
        n1 = max n 1
        n2 = min n1 count
        binSize = count `div` n2
        nbins =
            if binSize * n2 < count
            then n2 + 1
            else n2
     in assert (high >= low) (binFromSizeN low binSize nbins x)

-- Use binary search to find the bin
--
-- | Classify an input value to bins using the bin boundaries specified in an
-- array.
--
-- /Unimplemented/
--
{-# INLINE binBoundaries #-}
binBoundaries :: -- Integral a =>
    Array.Array a -> a -> HistBin a
binBoundaries = undefined

-- | Given a bin classifier function and a stream of values, generate a
-- histogram map from indices of bins to the number of items in the bin.
--
-- >>> Stream.fold (histogram (binOffsetSize 0 3)) $ Stream.fromList [1..15]
-- fromList [(0,2),(1,3),(2,3),(3,3),(4,3),(5,1)]
--
{-# INLINE histogram #-}
histogram :: (Monad m, Ord k) => (a -> k) -> Fold m a (Map k Int)
histogram bin = Fold.toMap bin Fold.length
