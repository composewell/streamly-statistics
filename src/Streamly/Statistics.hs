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
-- Measurements can be performed over the entire input stream or on a rolling
-- window of fixed or variable size.  Where possible, measures are computed
-- online without buffering the input stream.
--
-- Folds of type @Fold m (a, Maybe a) b@ are rolling window folds. An input of
-- type (a, Nothing) indicates that the input element is inserted in the window
-- increasing the window size by 1. An input of type (a, Just a) indicates that
-- the first element is being inserted in the window and the second element is
-- being removed from the window, the window size remains the same. The window
-- size can only increase and never decrease.
--
-- You can compute the statistics over the entire stream using rolling window
-- folds by supplying the second element of the input tuple as Nothing always.
--
-- Currently there is no overflow detection.
--
-- Reference:
--
-- * https://en.wikipedia.org/wiki/Statistics

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
    -- * Rolling Window Folds
      lmap
    , noSlide

    -- * Summary Statistics

    -- ** Sums
    , length
    , sum
    , sumInt
    , powerSum

    -- ** Location
    , min
    , max
    , rawMoment
    , rawMomentFrac

    -- Pythagorean means (https://en.wikipedia.org/wiki/Pythagorean_means)
    , mean
    , welfordMean
    , weightedMean
    , geometricMean
    , harmonicMean

    , quadraticMean

    -- Generalized mean
    , powerMean
    , powerMeanFrac

    -- ** Spread
    -- | Central moments are a statistical measure of dispersion.  The \(k\)th
    -- moment about the mean (or \(k\)th central moment) is defined as:
    --
    -- \(\mu_k = \frac{1}{n}\sum_{i=1}^n {(x_{i}-\mu)}^k\)
    --
    -- See https://mathworld.wolfram.com/CentralMoment.html .
    -- See https://en.wikipedia.org/wiki/Statistical_dispersion .
    , range

    , variance
    , stdDev
    , skewness
    , kurtosis

    -- XXX Move to Statistics.Sample module?
    -- ** Sampling
    , sampleVariance
    , sampleStdDev
    , stdErrMean
    )
where

import Control.Monad.IO.Class
import Data.Bifunctor(bimap)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Foreign.Storable

import Streamly.Data.Fold.Tee(Tee(..), toFold)
import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..))
import Streamly.Internal.Data.Ring.Foreign (slidingWindow)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Deque.Strict as DQ
import qualified Streamly.Data.Fold as Fold

import Prelude hiding (length, sum, min, max)


-- XXX Make the following more numerically stable. Try to extend welfordMean
-- XXX method.
-- XXX     - stdDev
-- XXX     - skewness
-- XXX     - kurtosis
-- XXX References:
-- XXX     * https://www.johndcook.com/blog/standard_deviation/
-- XXX     * https://www.johndcook.com/blog/skewness_kurtosis/
-- XXX     * Art of Computer Programming, Volume 2: Seminumerical Algorithms
-- XXX       (3rd Edition), Page 232
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

-- | Map a function on the incoming as well as outgoing element of a rolling
-- window fold.
--
-- >>> lmap f = Fold.lmap (bimap f (f <$>))
--
{-# INLINE lmap #-}
lmap :: (c -> a) -> Fold m (a, Maybe a) b -> Fold m (c, Maybe c) b
lmap f = Fold.lmap (bimap f (f <$>))

-- | Convert a rolling fold to a normal fold using the entire input stream as a
-- single window.
--
-- >>> noSlide f = Fold.lmap (, Nothing) f
--
{-# INLINE noSlide #-}
noSlide :: Fold m (a, Maybe a) b -> Fold m a b
noSlide f = Fold.lmap (, Nothing) f

-------------------------------------------------------------------------------
-- Location
-------------------------------------------------------------------------------

-- Theoretically, we can approximate minimum in a rolling window by using a
-- 'powerMean' with sufficiently large negative power.
--
-- | The minimum element in a rolling window.
--
-- If you want to compute the min of the entire stream Fold.min from streamly
-- package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE min #-}
min :: (Monad m, Ord a) => Fold m (a, Maybe a) a
min = Fold step initial extract

    where

    initial = return $ Partial $ Tuple3' (0 :: Int) (0 :: Int)
                (mempty :: DQ.Deque (Int, a))

    step (Tuple3' i w q) (a, ma) =
        case ma of
            Nothing ->
                return $ Partial $ Tuple3' (i + 1) (w + 1)
                    (headCheck i q (w + 1) & dqloop (i, a))
            Just _ ->
                return $ Partial $ Tuple3' (i + 1) w
                    (headCheck i q w & dqloop (i,a))

    {-# INLINE headCheck #-}
    headCheck i q w =
        case DQ.uncons q of
            Nothing -> q
            Just (ia', q') ->
                if fst ia' <= i - w
                then q'
                else q

    dqloop ia q =
        case DQ.unsnoc q of
            Nothing -> DQ.snoc ia q
            -- XXX This can be improved for the case of `=`
            Just (ia', q') ->
                if snd ia <= snd ia'
                then dqloop ia q'
                else DQ.snoc ia q

    extract (Tuple3' _ _ q) = return $ snd
                                $ fromMaybe (0, error "min: Empty stream")
                                $ DQ.head q

-- Theoretically, we can approximate maximum in a rolling window by using a
-- 'powerMean' with sufficiently large positive power.
--
-- | The maximum element in a rolling window.
--
-- If you want to compute the max of the entire stream Fold.max from streamly
-- package would be much faster.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE max #-}
max :: (Monad m, Ord a) => Fold m (a, Maybe a) a
max = Fold step initial extract

    where

    initial = return $ Partial $ Tuple3' (0 :: Int) (0 :: Int)
                (mempty :: DQ.Deque (Int, a))

    step (Tuple3' i w q) (a, ma) =
        case ma of
            Nothing ->
                return $ Partial $ Tuple3' (i + 1) (w + 1)
                    (headCheck i q (w + 1) & dqloop (i, a))
            Just _ ->
                return $ Partial $ Tuple3' (i + 1) w
                    (headCheck i q w & dqloop (i,a))

    {-# INLINE headCheck #-}
    headCheck i q w =
        case DQ.uncons q of
            Nothing -> q
            Just (ia', q') ->
                if fst ia' <= i - w
                then q'
                else q

    dqloop ia q =
        case DQ.unsnoc q of
        Nothing -> DQ.snoc ia q
        -- XXX This can be improved for the case of `=`
        Just (ia', q') ->
            if snd ia >= snd ia'
            then dqloop ia q'
            else DQ.snoc ia q

    extract (Tuple3' _ _ q) =
        return
            $ snd
            $ fromMaybe (0, error "max: Empty stream")
            $ DQ.head q

-- | The difference between the max and min elements of a rolling window.
--
-- >>> range = Fold.teeWith (-) max min
--
-- If you want to compute the range of the entire stream @Fold.teeWith (-)
-- Fold.max Fold.min@  from the streamly package would be much faster.
--
-- /Space/: \(\mathcal{O}(n)\) where @n@ is the window size.
--
-- /Time/: \(\mathcal{O}(n*w)\) where \(w\) is the window size.
--
{-# INLINE range #-}
range :: (Monad m, Num a, Ord a) => Fold m (a, Maybe a) a
range = Fold.teeWith (-) max min

-- XXX Overflow.
--
-- | The sum of all the elements in a rolling window. The input elements are
-- required to be intergal numbers.
--
-- This was written in the hope that it would be a tiny bit faster than 'sum'
-- for 'Integral' values. But turns out that 'sum' is 2% faster than this even
-- for intergal values!
--
-- /Internal/
--
{-# INLINE sumInt #-}
sumInt :: forall m a. (Monad m, Integral a) => Fold m (a, Maybe a) a
sumInt = Fold step initial extract

    where

    initial = return $ Partial (0 :: a)

    step s (a, ma) =
        return
            $ Partial
                $ case ma of
                    Nothing -> s + a
                    Just old -> s + a - old

    extract = return

-- XXX Overflow.
--
-- | Sum of all the elements in a rolling window:
--
-- \(S = \sum_{i=1}^n x_{i}\)
--
-- This is the first power sum.
--
-- >>> sum = powerSum 1
--
-- Uses Kahan-Babuska-Neumaier style summation for numerical stability of
-- floating precision arithmetic.
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
--
{-# INLINE sum #-}
sum :: forall m a. (Monad m, Num a) => Fold m (a, Maybe a) a
sum = Fold step initial extract

    where

    initial =
        return
            $ Partial
            $ Tuple'
                (0 :: a) -- running sum
                (0 :: a) -- accumulated rounding error

    step (Tuple' total err) (new, mOld) =
        let incr =
                case mOld of
                    -- XXX new may be large and err may be small we may lose it
                    Nothing -> new - err
                    -- XXX if (new - old) is large we may lose err
                    Just old -> (new - old) - err
            -- total is large and incr may be small, we may round incr here but
            -- we will accumulate the rounding error in err1 in the next step.
            total1 = total + incr
            -- Accumulate any rounding error in err1
            -- XXX In the Nothing case above we may lose err, therefore we
            -- should use ((total1 - total) - new) + err here.
            -- Or even in the just case if (new - old) is large we may lose
            -- err, so we should use ((total1 - total) + (old - new)) + err.
            err1 = (total1 - total) - incr
        in return $ Partial $ Tuple' total1 err1

    extract (Tuple' total _) = return total

-- | The number of elements in the rolling window.
--
-- This is the \(0\)th power sum.
--
-- >>> length = powerSum 0
--
{-# INLINE length #-}
length :: (Monad m, Num b) => Fold m (a, Maybe a) b
length = Fold.foldl' step 0

    where

    step w (_, Nothing) = w + 1
    step w _ = w

-- | Arithmetic mean of elements in a rolling window:
--
-- \(\mu = \frac{\sum_{i=1}^n x_{i}}{n}\)
--
-- Mean is the first raw moment.
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
mean = Fold.teeWith (/) sum length

-- | Sum of the \(k\)th power of all the elements in a rolling window:
--
-- \(S_k = \sum_{i=1}^n x_{i}^k\)
--
-- >>> powerSum k = lmap (^ k) sum
--
-- /Space/: \(\mathcal{O}(1)\)
--
-- /Time/: \(\mathcal{O}(n)\)
{-# INLINE powerSum #-}
powerSum :: (Monad m, Num a) => Int -> Fold m (a, Maybe a) a
powerSum k = lmap (^ k) sum

-- | Like 'powerSum' but powers can be negative or fractional. This is slower
-- than 'powerSum' for positive intergal powers.
--
-- >>> powerSumFrac p = lmap (** p) sum
--
{-# INLINE powerSumFrac #-}
powerSumFrac :: (Monad m, Floating a) => a -> Fold m (a, Maybe a) a
powerSumFrac p = lmap (** p) sum

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
rawMoment k = Fold.teeWith (/) (powerSum k) length

-- | Like 'rawMoment' but powers can be negative or fractional. This is
-- slower than 'rawMoment' for positive intergal powers.
--
-- >>> rawMomentFrac p = Fold.teeWith (/) (powerSumFrac p) length
--
{-# INLINE rawMomentFrac #-}
rawMomentFrac :: (Monad m, Floating a) => a -> Fold m (a, Maybe a) a
rawMomentFrac k = Fold.teeWith (/) (powerSumFrac k) length

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
harmonicMean = Fold.teeWith (/) length (lmap recip sum)

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

-- | The variance \(\sigma^2\) of a population of \(n\) equally likely values
-- is defined as the average of the squares of deviations from the mean
-- \(\mu\). In other words, second moment about the mean:
--
-- \(\sigma^2 = \frac{1}{n}\sum_{i=1}^n {(x_{i}-\mu)}^2\)
--
-- \(\sigma^2 = rawMoment(2) - \mu^2\)
--
-- \(\mu_2 = -\mu\'_1^2 + \mu'_2\)
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
sampleVariance = Fold.teeWith (\n s2 -> n * s2 / (n - 1)) length variance

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
stdErrMean = Fold.teeWith (\sd n -> sd / sqrt n) sampleStdDev length

-- | Skewness \(\gamma\) is the standardized third central moment. The third
-- central moment can be computed in terms of raw moments:
--
-- \(\mu_3 = 2\mu\'_1^3 - 3\mu'_1\mu'_2 + \mu'_3\)
--
-- It is 0 for a symmetric distribution, negative for a distribution that is
-- skewed towards left, positive for a distribution skewed towards right.
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
    toFold $ (\p3 sd m -> p3 / sd ^ (3 :: Int) - 3 * (m / sd)
                - (m / sd) ^ (3 :: Int))
    <$> Tee (rawMoment 3)
    <*> Tee stdDev
    <*> Tee mean

-- XXX We can compute the 2nd, 3rd, 4th raw moments by repeatedly multiplying
-- instead of computing the powers every time.
--
-- | Kurtosis \(\kappa\) is the standardized fourth central moment. The fourth
-- central moment can be computed in terms of raw moments:
--
-- \(\mu_4 = -3\mu\'_1^4 + 6\mu\'_1^2\mu'_2 - 4\mu'_1\mu'_3\ + \mu'_4\)
--
-- It is always non-negative. It is 0 for a point distribution, low for light
-- tailed (platykurtic) distributions and high for heavy tailed (leptokurtic)
-- distributions.
--
-- For a normal distribution \(\kappa = 3\sigma^4\).
--
-- See https://en.wikipedia.org/wiki/Kurtosis .
{-# INLINE kurtosis #-}
kurtosis :: (Monad m, Floating a) => Fold m (a, Maybe a) a
kurtosis =
    toFold $
    (\p4 p3 sd m ->
        p4 / sd ^ (4 :: Int) - 4 * ((m / sd) * (p3 / sd ^ (3 :: Int))) -
            3 * ((m / sd) ^ (4 :: Int)))
    <$> Tee (rawMoment 4)
    <*> Tee (rawMoment 3)
    <*> Tee stdDev
    <*> Tee mean

-- | Same as 'mean' but uses Welford's algorithm to compute the mean
-- incrementally.
--
-- It maintains a running mean instead of a running sum and adjusts the mean
-- based on a new value.  This is slower than 'mean' because of using the
-- division operation on each step and it is numerically unstable. The
-- advantage over 'mean' could be no overflow if the numbers are large, because
-- we do not maintain a sum, but that is a highly unlikely corner case.
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

-- | Geometric mean, defined as:
--
-- \(GM = \sqrt[n]{x_1 x_2 \cdots x_n}\)
--
-- \(GM = \left(\prod_{i=1}^n x_i\right)^\frac{1}{n}\)
--
-- or, equivalently, as the arithmetic mean in logspace:
--
-- \(GM = e ^{{\frac{\sum_{i=1}^{n}\ln a_i}{n}}}\)
--
-- >>> geometricMean = exp <$> lmap log mean
--
-- See https://en.wikipedia.org/wiki/Geometric_mean .
{-# INLINE geometricMean #-}
geometricMean :: forall m a. (Monad m, Floating a) => Fold m (a, Maybe a) a
geometricMean = exp <$> Fold.lmap (bimap log (log <$>)) mean

-- | @weightedMean@ computes the weighted mean of the sample data.
--
-- The weights should add up to 1. It uses Kahan-Babuska-Neumaier summation.
--
{-# INLINE weightedMean #-}
weightedMean :: forall m a. (MonadIO m, Fractional a, Storable a)
    => Int -> Fold m (a, a) a
weightedMean n = Fold.lmap (uncurry (*)) (slidingWindow n sum)
