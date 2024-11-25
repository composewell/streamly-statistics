-- |
-- Module      : Streamly.Statistics.Scanl
-- Copyright   : (c) 2024 Composewell Technologies
-- License     : Apache-2.0
-- Maintainer  : streamly@composewell.com
-- Stability   : experimental
-- Portability : GHC
--
-- See "Streamly.Statistics" for general information. This module provides
-- scans instead of folds.

{-# LANGUAGE ScopedTypeVariables #-}
module Streamly.Statistics.Scanl
    (
    -- * Incremental Scans
    -- | Scans of type @Scanl m (a, Maybe a) b@ are incremental sliding window
    -- scans. An input of type @(a, Nothing)@ indicates that the input element
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
    -- Also see "Streamly.Data.Scanl" for some basic window scans.

    -- * Summary Statistics
    -- | See https://en.wikipedia.org/wiki/Summary_statistics .

    -- ** Location
    -- | See https://en.wikipedia.org/wiki/Location_parameter .
    --
    -- See https://en.wikipedia.org/wiki/Central_tendency .
      incrMinimum
    , incrMaximum
    , incrRawMoment
    , incrRawMomentFrac

    -- Pythagorean means (https://en.wikipedia.org/wiki/Pythagorean_means)
    , incrWelfordMean
    , incrGeometricMean
    , incrHarmonicMean

    , incrQuadraticMean

    -- Generalized mean
    , incrPowerMean
    , incrPowerMeanFrac

    -- ** Weighted Means
    -- | Exponential Smoothing.
    , ewma
    , ewmaRampUpSmoothing
    , incrEwma

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
    , incrRange
    , incrMd
    , incrVariance
    , incrStdDev

    -- ** Shape
    -- | Third and fourth order central moments are a measure of shape.
    --
    -- See https://en.wikipedia.org/wiki/Shape_parameter .
    --
    -- See https://en.wikipedia.org/wiki/Standardized_moment .
    , incrSkewness
    , incrKurtosis

    -- XXX Move to Statistics.Sample or Statistics.Estimation module?
    -- ** Estimation
    , incrSampleVariance
    , incrSampleStdDev
    , incrStdErrMean

    -- ** Probability Distribution
    , incrFrequency
    )
where

import Control.Exception (assert)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Function ((&))
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Streamly.Internal.Data.Fold (Step(..))
import Streamly.Internal.Data.Scanl (Scanl(..), Incr(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Data.Map.Strict as Map
import qualified Deque.Strict as Deque
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.RingArray as Ring
import qualified Streamly.Internal.Data.Scanl as Scanl
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
-- Location
-------------------------------------------------------------------------------

-- Theoretically, we can approximate minimum in a rolling window by using a
-- 'powerMean' with sufficiently large negative power.
--
-- XXX If we need to know the minimum in the window only once in a while then
-- we can use linear search when it is extracted and not pay the cost all the
-- time.

-- | The minimum element in a rolling window.
--
-- For smaller window sizes (< 30) Streamly.Internal.Data.Fold.windowMinimum performs
-- better.  If you want to compute the minimum of the entire stream Fold.min
-- from streamly package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE incrMinimum #-}
incrMinimum :: (Monad m, Ord a) => Scanl m (Incr a) a
incrMinimum = Scanl step initial extract extract

    where

    initial =
        return
            $ Partial
            $ Tuple3' (0 :: Int) (0 :: Int) (mempty :: Deque.Deque (Int, a))

    step (Tuple3' i w q) (Insert a) =
                return
                    $ Partial
                    $ Tuple3'
                        (i + 1)
                        (w + 1)
                        (headCheck i q (w + 1) & dqloop (i, a))

    step (Tuple3' i w q) (Replace new _) =
        return
            $ Partial
            $ Tuple3' (i + 1) w (headCheck i q w & dqloop (i, new))

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

-- | The maximum element in a rolling window.
--
-- For smaller window sizes (< 30) Streamly.Internal.Data.Fold.windowMaximum
-- performs better.  If you want to compute the maximum of the entire stream
-- Streamly.Data.Fold.maximum from streamly package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE incrMaximum #-}
incrMaximum :: (Monad m, Ord a) => Scanl m (Incr a) a
incrMaximum = Scanl step initial extract extract

    where

    initial =
        return
            $ Partial
            $ Tuple3' (0 :: Int) (0 :: Int) (mempty :: Deque.Deque (Int, a))

    step (Tuple3' i w q) (Insert a) =
        return
            $ Partial
            $ Tuple3'
                (i + 1)
                (w + 1)
                (headCheck i q (w + 1) & dqloop (i, a))

    step (Tuple3' i w q) (Replace new _) =
        return
            $ Partial
            $ Tuple3' (i + 1) w (headCheck i q w & dqloop (i, new))

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

-- | Recompute mean from old mean when an item is added to the sample.
{-# INLINE meanAdd #-}
meanAdd :: Fractional a => Int -> a -> a -> a
meanAdd n oldMean newItem =
    let delta = (newItem - oldMean) / fromIntegral (n + 1)
     in oldMean + delta

-- We do not carry rounding errors, therefore, this would be less numerically
-- stable than the kbn mean.

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
{-# INLINE incrWelfordMean #-}
incrWelfordMean :: forall m a. (Monad m, Fractional a) => Scanl m (Incr a) a
incrWelfordMean = Scanl step initial extract extract

    where

    initial =
        return
            $ Partial
            $ Tuple'
                (0 :: a)   -- running mean
                (0 :: Int) -- count of items in the window

    step (Tuple' oldMean w) (Insert new) =
        return $ Partial $ Tuple' (meanAdd w oldMean new) (w + 1)

    step (Tuple' oldMean w) (Replace new old) =
        return $ Partial $ Tuple' (meanReplace w oldMean old new) w

    extract (Tuple' x _) = return x

-------------------------------------------------------------------------------
-- Moments
-------------------------------------------------------------------------------

-- XXX We may have chances of overflow if the powers are high or the numbers
-- are large. A limited mitigation could be to use welford style avg
-- computation. Do we need an overflow detection?

-- | Raw moment is the moment about 0. The \(k\)th raw moment is defined as:
--
-- \(\mu'_k = \frac{\sum_{i=1}^n x_{i}^k}{n}\)
--
-- >>> rawMoment k = Fold.teeWith (/) (Fold.windowPowerSum p) Fold.windowLength
--
-- See https://en.wikipedia.org/wiki/Moment_(mathematics) .
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE incrRawMoment #-}
incrRawMoment :: (Monad m, Fractional a) => Int -> Scanl m (Incr a) a
incrRawMoment k =
    Scanl.teeWith (/) (Scanl.incrPowerSum k) Scanl.incrCount

-- | Like 'rawMoment' but powers can be negative or fractional. This is
-- slower than 'rawMoment' for positive intergal powers.
--
-- >>> rawMomentFrac p = Fold.teeWith (/) (Fold.windowPowerSumFrac p) Fold.windowLength
--
{-# INLINE incrRawMomentFrac #-}
incrRawMomentFrac :: (Monad m, Floating a) => a -> Scanl m (Incr a) a
incrRawMomentFrac k =
    Scanl.teeWith (/) (Scanl.incrPowerSumFrac k) Scanl.incrCount

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
{-# INLINE incrPowerMean #-}
incrPowerMean :: (Monad m, Floating a) => Int -> Scanl m (Incr a) a
incrPowerMean k = (** (1 / fromIntegral k)) <$> incrRawMoment k

-- | Like 'powerMean' but powers can be negative or fractional. This is
-- slower than 'powerMean' for positive intergal powers.
--
-- >>> powerMeanFrac k = (** (1 / k)) <$> rawMomentFrac k
--
{-# INLINE incrPowerMeanFrac #-}
incrPowerMeanFrac :: (Monad m, Floating a) => a -> Scanl m (Incr a) a
incrPowerMeanFrac k = (** (1 / k)) <$> incrRawMomentFrac k

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
{-# INLINE incrHarmonicMean #-}
incrHarmonicMean :: (Monad m, Fractional a) => Scanl m (Incr a) a
incrHarmonicMean =
    Scanl.teeWith (/)
        Scanl.incrCount (Scanl.lmap (fmap recip) Scanl.incrSum)

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
{-# INLINE incrGeometricMean #-}
incrGeometricMean :: (Monad m, Floating a) => Scanl m (Incr a) a
incrGeometricMean = exp <$> Scanl.lmap (fmap log) Scanl.incrMean

-- | The quadratic mean or root mean square (rms) of the numbers
-- \(x_1, x_2, \ldots, x_n\) is defined as:
--
-- \(RMS = \sqrt{ \frac{1}{n} \left( x_1^2 + x_2^2 + \cdots + x_n^2 \right) }.\)
--
-- >>> quadraticMean = powerMean 2
--
-- See https://en.wikipedia.org/wiki/Root_mean_square .
--
{-# INLINE incrQuadraticMean #-}
incrQuadraticMean :: (Monad m, Floating a) => Scanl m (Incr a) a
incrQuadraticMean = incrPowerMean 2

-------------------------------------------------------------------------------
-- Weighted Means
-------------------------------------------------------------------------------

-- XXX Is this numerically stable? We can use the kbn summation here.
-- XXX change the signature to use the newer value first.

-- | ewmaStep smoothing-factor old-value new-value
--
-- >>> ewmaStep k x0 x1 = k * x1 + (1 - k) * x0
--
{-# INLINE ewmaStep #-}
ewmaStep :: Double -> Double -> Double -> Double
ewmaStep k x0 x1 = (1 - k) * x0 + k * x1

-- XXX Compute this in a sliding window?

-- | Exponentially weighted moving average is given by @ewma w@ where @w@ is
-- the smoothing factor varying between 0 and 1. To compute the moving average
-- the newest input is given a weight of @w@ and the running ewma is given a
-- weight of @1 - w@.
--
-- For an empty stream this API returns 0. For a non-empty stream the first
-- value in the stream is used as the initial ewma.
--
-- The higher the smoothing factor the more weight is given to the new value.
-- Consider some interesting special cases, when @w@ is 1 the ewma is always
-- the newest value, when @w@ is 0 then the ewma is always the oldest value. If
-- @w@ is @0.5@ then the new inputs get exponentially weighted by weights
-- @1\/2, 1\/4, 1\/8, ...@, see below for details.
--
-- Mathematically, we define ewma \(s_n\), of \(n\) values, \(x_1,\ldots,x_n\),
-- recursively as follows:
--
-- \(\begin{align} s_0& = x_0, \quad \text{initial value}\\ s_n & = \alpha x_{n} + (1-\alpha)s_{n-1},\quad n>0 \end{align}\)
--
-- If we expand the recursive term it reveals an exponential series:
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
ewma :: Monad m => Double -> Scanl m Double Double
ewma k = extract <$> Scanl.mkScanl step (Tuple' 0 1)

    where

    step (Tuple' x0 k1) x = Tuple' (ewmaStep k1 x0 x) k

    extract (Tuple' x _) = x

-- | @ewma n k@ is like 'ewma' but the smoothing factor used is itself
-- exponentially smoothened starting from @1@ to the final value @k@ using @n@
-- as its smoothing factor. In other words, the smoothing factor is derived by
-- smoothing the series @1,k,k,k,...@ using @n@ as the smoothing factor. As
-- time goes on, the smoothing factor gets closer to k.
--
{-# INLINE ewmaRampUpSmoothing #-}
ewmaRampUpSmoothing :: Monad m => Double -> Double -> Scanl m Double Double
ewmaRampUpSmoothing n k1 = extract <$> Scanl.mkScanl step initial

    where

    initial = Tuple' 0 1

    step (Tuple' x0 k0) x1 =
        let x = ewmaStep k0 x0 x1
            k = ewmaStep n k0 k1
        in Tuple' x k

    extract (Tuple' x _) = x

data Ewma = EwmaInit | EwmaGo !Double !Double

-- | @incrEwma w@ computes the ewma of the elements incrementally in a window
-- using @w@ as the smoothing factor.
--
-- ewma can be calculated incrementally as follows. If \(x_i\) is the value
-- entering the window, n is the size of window, \(x_1\) is the second last value
-- in the old window and \(x_0\) is the value exiting the window:
--
-- \(s_{new} = \alpha x_i + (1-\alpha)s_{old} + (1-\alpha)^{n} (x_1 - x_0\)\)
--
-- /Unimplemented/
--
{-# INLINE incrEwma #-}
incrEwma :: MonadIO m =>
    Double -> Scanl m (Incr Double, Ring.RingArray Double) Double
incrEwma w = Scanl step initial extract extract

    where

    initial = do
        when (w < 0 || w > 1)
            $ error "incrEwma: weight must be >= 0 and <= 1"
        return $ Partial EwmaInit

    step EwmaInit (Insert x, rng) = do
        let len = Ring.length rng
        assert (len == 0) (return ())
        return $ Partial (EwmaGo x 1)

    step EwmaInit _ = error "incrEwma: the first window operation must be Insert"

    step (EwmaGo s k) (Insert x, _) = do
        let s1 = w * x + (1 - w) * s
        return $ Partial (EwmaGo s1 (k * (1 - w)))

    step (EwmaGo s k) (Replace x x0, rng) = do
        x1 <- Ring.unsafeGetIndex 0 rng
        let s1 = w * x + (1 - w) * s + k * (x1 - x0)
        return $ Partial (EwmaGo s1 k)

    extract EwmaInit = return 0
    extract (EwmaGo x _) = return x

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
{-# INLINE incrRange #-}
incrRange :: (Monad m, Num a, Ord a) => Scanl m (Incr a) a
incrRange = Scanl.teeWith (-) incrMaximum incrMinimum

-- | @md n@ computes the mean absolute deviation (or mean deviation) in a
-- sliding window of last @n@ elements in the stream.
--
-- The input of the scan is (incr, ring), where incr is the incremental window
-- operation and ring is the contents of the entire window in a ring array.
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
{-# INLINE incrMd #-}
incrMd ::  MonadIO m =>
    Scanl m (Incr Double, Ring.RingArray Double) Double
incrMd =
    Scanl.rmapM computeMD
        $ Scanl.tee
            (Scanl.lmap fst Scanl.incrMean) (Scanl.lmap snd Scanl.latest)

    where

    computeMD (mn, mRng) =
        case mRng of
            Just rng -> do
                Stream.fold Fold.mean
                    $ fmap (\a -> abs (mn - a))
                    $ Ring.read rng
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
{-# INLINE incrVariance #-}
incrVariance :: (Monad m, Fractional a) => Scanl m (Incr a) a
incrVariance =
    Scanl.teeWith
        (\p2 m -> p2 - m ^ (2 :: Int)) (incrRawMoment 2) Scanl.incrMean

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
{-# INLINE incrStdDev #-}
incrStdDev :: (Monad m, Floating a) => Scanl m (Incr a) a
incrStdDev = sqrt <$> incrVariance

-- XXX Need a tee3 operation for better performance.

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
{-# INLINE incrSkewness #-}
incrSkewness :: (Monad m, Floating a) => Scanl m (Incr a) a
incrSkewness =
          (\rm3 sd mu ->
            rm3 / sd ^ (3 :: Int) - 3 * (mu / sd) - (mu / sd) ^ (3 :: Int)
          )
        <$> incrRawMoment 3
        <*> incrStdDev
        <*> Scanl.incrMean

-- XXX We can compute the 2nd, 3rd, 4th raw moments by repeatedly multiplying
-- instead of computing the powers every time.
-- XXX Need a tee4 operation for better performance.

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
{-# INLINE incrKurtosis #-}
incrKurtosis :: (Monad m, Floating a) => Scanl m (Incr a) a
incrKurtosis =
          (\rm4 rm3 sd mu ->
             ( 3 * mu ^ (4 :: Int)
            + 6 * mu ^ (2 :: Int) * sd ^ (2 :: Int)
            - 4 * mu * rm3
            + rm4) / (sd ^ (4 :: Int))
          )
        <$> incrRawMoment 4
        <*> incrRawMoment 3
        <*> incrStdDev
        <*> Scanl.incrMean

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
{-# INLINE incrSampleVariance #-}
incrSampleVariance :: (Monad m, Fractional a) => Scanl m (Incr a) a
incrSampleVariance =
    Scanl.teeWith (\n s2 -> n * s2 / (n - 1)) Scanl.incrCount incrVariance

-- | Sample standard deviation:
--
-- \(s = \sqrt{sampleVariance}\)
--
-- >>> sampleStdDev = sqrt <$> sampleVariance
--
-- See https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
-- .
--
{-# INLINE incrSampleStdDev #-}
incrSampleStdDev :: (Monad m, Floating a) => Scanl m (Incr a) a
incrSampleStdDev = sqrt <$> incrSampleVariance

-- | Standard error of the sample mean (SEM), defined as:
--
-- \( SEM = \frac{sampleStdDev}{\sqrt{n}} \)
--
-- See https://en.wikipedia.org/wiki/Standard_error .
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE incrStdErrMean #-}
incrStdErrMean :: (Monad m, Floating a) => Scanl m (Incr a) a
incrStdErrMean =
    Scanl.teeWith (\sd n -> sd / sqrt n) incrSampleStdDev Scanl.incrCount

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
{-# INLINE incrFrequency #-}
incrFrequency :: (Monad m, Ord a) => Scanl m (Incr a) (Map a Int)
incrFrequency = Scanl.mkScanl step Map.empty

    where

    decrement v =
        if v == 1
        then Nothing
        else Just (v - 1)

    step refCountMap (Insert new) =
        Map.insertWith (+) new 1 refCountMap

    step refCountMap (Replace new old) =
        let m1 = Map.insertWith (+) new 1 refCountMap
         in Map.update decrement old m1
