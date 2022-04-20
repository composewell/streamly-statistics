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
    , Window.minimum
    , Window.maximum
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
    , Window.range
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
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Streamly.Data.Fold.Tee(Tee(..), toFold)
import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..))

import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Array.Foreign.Mut as MA
import qualified Streamly.Internal.Data.Fold.Window as Window
import qualified Streamly.Internal.Data.Stream.IsStream as Stream

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
-- Utilities
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
                (0 :: Int) -- count of numbers in the window

    step (Tuple' x w) (y, my) =
        return
            $ Partial
            $ case my of
                Nothing ->
                    let w1 = fromIntegral (w + 1)
                     in Tuple' (x + (y - x) / w1) (w + 1)
                Just old ->
                    let w1 = fromIntegral w
                    -- XXX can we use x + (y - old) / w1 ?
                    -- XXX We can carry the rounding errors to provide
                    -- numerical stability like in 'mean'.
                     in Tuple' (x + (y - x) / w1 + (x - old) / w1) w

    extract (Tuple' x _) = return x

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
md ::  MonadIO m => Fold m ((Double, Maybe Double), m (MA.Array Double)) Double
md =
    Fold.rmapM computeMD
        $ Fold.tee (Fold.lmap fst mean) (Fold.lmap snd Fold.last)

    where

    computeMD (mn, rng) =
        case rng of
            Just action -> do
                arr <- action
                Stream.fold Fold.mean
                    $ Stream.map (\a -> abs (mn - a))
                    $ Stream.unfold MA.read arr
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
    toFold
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
-- /Broken/
{-# INLINE kurtosis #-}
kurtosis :: (Monad m, Floating a) => Fold m (a, Maybe a) a
kurtosis =
    toFold
        $ (\rm4 rm3 sd mu ->
              rm4 / sd ^ (4 :: Int)
            - 4 * ((mu / sd) * (rm3 / sd ^ (3 :: Int)))
            - 3 * ((mu / sd) ^ (4 :: Int))
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
