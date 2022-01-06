{-# LANGUAGE ScopedTypeVariables #-}

module Streamly.Statistics
    (
    -- * Descriptive functions
      min
    , max
    , range

    -- * Statistics of a location
    , kurtosis
    , geometricMean
    , skewness
    , stdDev
    , stdErrMean
    , sum
    , sumInt
    , mean
    , welfordMean
    , weightedMean
    )
where

import Control.Monad.IO.Class (MonadIO(..))
import Data.Bifunctor(bimap)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.Storable

import Streamly.Data.Fold.Tee(Tee(..), toFold)
import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..))
import Streamly.Internal.Data.Ring.Foreign (slidingWindow)
import Streamly.Internal.Data.Tuple.Strict (Tuple'(..), Tuple3'(..))

import qualified Deque.Strict as DQ
import qualified Streamly.Data.Fold as Fold

import Prelude hiding (sum, min, max)

instance Storable (Double, Double) where
    sizeOf _ = 2 * sizeOf (undefined :: Double)
    alignment _ = 16
    peek p = do
        let q = castPtr p
        a <- peek q
        b <- peek (q `plusPtr` sizeOf a)
        return (a, b)
    poke p (a, b) = do
        let q = castPtr p
        poke q a
        poke (q `plusPtr` sizeOf a) b

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

data Tuple5' a b c d e = Tuple5' !a !b !c !d !e deriving Show

-- | The minimum element in the window.
--
-- /Time complexity/: @O(n^2)@ where @n@ is the window size.
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

-- | The maximum element in the window.
--
-- /Time complexity/: @O(n^2)@ where @n@ is the window size.
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

-- | Range. The difference between the largest and smallest elements of a
-- window.
{-# INLINE range #-}
range :: (Monad m, Num a, Ord a) => Fold m (a, Maybe a) a
range = Fold.teeWith (-) max min

-- | The sum of all the elements in the window.
{-# INLINE sumInt #-}
sumInt :: forall m a. (MonadIO m, Num a) => Fold m (a, Maybe a) a
sumInt = Fold step initial extract

    where

    initial = return $ Partial (0 :: a)

    step s (a, ma) = return $ Partial $
        case ma of
            Nothing -> s + a
            Just old -> s + a - old

    extract = return

-- | The sum of all the elements in the window. This uses Kahan-Babuska-Neumaier
-- summation.
{-# INLINE sum #-}
sum :: forall m a. (MonadIO m, Num a) => Fold m (a, Maybe a) a
sum = Fold step initial extract

    where

    initial = return $ Partial $ Tuple' (0 :: a) (0 :: a)

    step (Tuple' s c) (a, ma) =
        let y =
                case ma of
                    Nothing -> a - c
                    Just old -> a - old - c
            t = s + y
            c1 = (t - s) - y
        in return $ Partial $ Tuple' t c1

    extract (Tuple' s _) = return s

-- | Window Size. It computes the size of the sliding window.
{-# INLINE windowSize #-}
windowSize :: MonadIO m => Fold m (a, Maybe a) Int
windowSize = Fold.foldl' step initial

    where

    initial = 0 :: Int

    step w (_, ma) =
        case ma of
            Nothing -> w + 1
            _ -> w

-- | Arithmetic mean. This uses Kahan-Babuska-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
{-# INLINE mean #-}
mean :: forall m a. (MonadIO m, Fractional a) => Fold m (a, Maybe a) a
mean = Fold.teeWith (/) sum (fromIntegral <$> windowSize)

{-# INLINE powerSum #-}
powerSum :: forall m a. (MonadIO m, Num a) => Int -> Fold m (a, Maybe a) a
powerSum i = Fold.lmap (\(a, ma) -> (a ^ i, (^i) <$> ma)) sum

{-# INLINE powerSumAvg #-}
powerSumAvg :: forall m a. (MonadIO m, Fractional a)
    => Int -> Fold m (a, Maybe a) a
powerSumAvg i = Fold.teeWith (/) (powerSum i) (fmap fromIntegral windowSize)

{-# INLINE variance #-}
variance :: forall m a. (MonadIO m, Fractional a) => Fold m (a, Maybe a) a
variance = Fold.teeWith (\p2 m -> p2 - m ^ (2 :: Integer)) (powerSumAvg 2) mean

{-# INLINE stdDev #-}
stdDev :: forall m a. (MonadIO m, Floating a) => Fold m (a, Maybe a) a
stdDev = sqrt <$> variance

{-# INLINE stdErrMean #-}
stdErrMean :: forall m a. (MonadIO m, Floating a) => Fold m (a, Maybe a) a
stdErrMean =
    Fold.teeWith
        (\sd n -> sd / sqrt n)
        stdDev
        (fmap fromIntegral windowSize)

{-# INLINE skewness #-}
skewness :: forall m a. (MonadIO m, Floating a) => Fold m (a, Maybe a) a
skewness =
    toFold $ (\p3 sd m -> p3 / sd ^ (3 :: Int) - 3 * (m / sd)
                - (m / sd) ^ (3 :: Int))
    <$> Tee (powerSumAvg 3)
    <*> Tee stdDev
    <*> Tee mean

{-# INLINE kurtosis #-}
kurtosis :: forall m a. (MonadIO m, Floating a) => Fold m (a, Maybe a) a
kurtosis =
    toFold $
    (\p4 p3 sd m ->
        p4 / sd ^ (4 :: Int) - 4 * ((m / sd) * (p3 / sd ^ (3 :: Int))) -
            3 * ((m / sd) ^ (4 :: Int)))
    <$> Tee (powerSumAvg 4)
    <*> Tee (powerSumAvg 3)
    <*> Tee stdDev
    <*> Tee mean

-- | Arithmetic mean. This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
--
-- Compared to 'mean', this loses a surprising amount of precision
-- unless the inputs are very large.
{-# INLINE welfordMean #-}
welfordMean :: forall m a. (MonadIO m, Floating a) => Fold m (a, Maybe a) a
welfordMean = Fold step initial extract

    where

    initial = return $ Partial $ Tuple' (0 :: a) (0 :: Int)

    step (Tuple' x w) (y, my) =
        return $
            case my of
                Nothing -> Partial $ Tuple' (x + (y - x) / w') (w + 1)
                            where w' = fromIntegral (w + 1)
                Just old -> Partial $
                                Tuple' (x + (y - x) / w' + (x - old) / w') w
                            where w' = fromIntegral w

    extract (Tuple' x _) = return x

{-# INLINE geometricMean #-}
geometricMean :: forall m a. (MonadIO m, Floating a) => Fold m (a, Maybe a) a
geometricMean = exp <$> Fold.lmap (bimap log (log <$>)) mean

-- | @weightedMean@ computes the weighted mean of the sample data.
--
-- The weights should add up to 1. It uses Kahan-Babuska-Neumaier summation.
--
{-# INLINE weightedMean #-}
weightedMean :: forall m a. (MonadIO m, Fractional a, Storable a)
    => Int -> Fold m (a, a) a
weightedMean n = Fold.lmap (uncurry (*)) (slidingWindow n sum)
