module Streamly.Statistics
  (
  -- * Types
    WindowSize(..)

  -- * Descriptive functions
  , min
  , max
  , range

  -- * Statistics of a location
  , sum
  , mean
  , welfordMean
  ) where

import Streamly.Internal.Data.Fold.Type (Fold(..), Step(..))
import Streamly.Internal.Data.Tuple.Strict
import Streamly.Data.Fold.Tee(Tee(..), toFold)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable(..))

import qualified Streamly.Prelude as Stream
import qualified Streamly.Internal.Data.Ring.Foreign as Ring
import qualified Streamly.Data.Fold as Fold
import qualified Streamly.Internal.Data.Fold as Fold
import Data.Function ((&))

import qualified Deque.Strict as DQ

import Prelude hiding (sum, min, max)
import qualified Prelude as P
import Data.Maybe (fromMaybe)
import Data.Bifunctor(bimap)

-- XXX Make the following more numerically stable. Try to extend welfordMean method.
-- XXX     - stdDev
-- XXX     - skewness
-- XXX     - kurtosis
-- XXX Refrences:
-- XXX     * https://www.johndcook.com/blog/standard_deviation/
-- XXX     * https://www.johndcook.com/blog/skewness_kurtosis/
-- XXX     * Art of Computer Programming, Volume 2: Seminumerical Algorithms (3rd Edition), Page 232


-- | The window size for the statistics to take place in.
-- If the window size is Finite i then the sample for statistics is
-- the last i elements seen in the stream else the sample is the
-- entire stream.
data WindowSize
  = Finite Int
  | Infinite

data Tuple5' a b c d e = Tuple5' !a !b !c !d !e deriving Show

-- | The minimum element in the window.
--
-- /Time complexity/: @O(n^2)@ where @n@ is the window size.
{-# INLINE min #-}
min :: (Monad m, Num a, Ord a, Storable a) => Fold m (a, Maybe a) a
min = Fold step initial extract

  where

  initial = return $ Partial $ Tuple3' (0 :: Int) (0 :: Int)
                (mempty :: DQ.Deque (Int, a))

  step (Tuple3' i w q) (a, ma) =
    case ma of
      Nothing ->
        return $ Partial $ Tuple3' (i + 1) (w + 1)
            (headCheck i q (w + 1) & dqloop (i, a))
      Just old ->
        return $ Partial $ Tuple3' (i + 1) w (headCheck i q w & dqloop (i,a))

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
max :: (Monad m, Num a, Ord a, Storable a) => Fold m (a, Maybe a) a
max = Fold step initial extract

  where

  initial = return $ Partial $ Tuple3' (0 :: Int) (0 :: Int)
                (mempty :: DQ.Deque (Int, a))

  step (Tuple3' i w q) (a, ma) =
    case ma of
      Nothing ->
        return $ Partial $ Tuple3' (i + 1) (w + 1)
            (headCheck i q (w + 1) & dqloop (i, a))
      Just old ->
        return $ Partial $ Tuple3' (i + 1) w (headCheck i q w & dqloop (i,a))

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

  extract (Tuple3' _ _ q) = return $ snd
                            $ fromMaybe (0, error "max: Empty stream")
                            $ DQ.head q

-- | Range. The difference between the largest and smallest elements of a
-- window.
{-# INLINE range #-}
range :: (Monad m, Num a, Ord a, Storable a) => Fold m (a, Maybe a) a
range = Fold.teeWith (-) max min

-- | The sum of all the elements in the sample. This uses Kahan-Babuska-Neumaier
-- summation.
{-# INLINE sum #-}
sum :: MonadIO m => Fold m (Double, Maybe Double) Double
sum = Fold step initial extract

  where

  initial = return $ Partial (0 :: Double)

  step s (a, ma) = return $ Partial $
        case ma of
          Nothing -> s + a
          Just old -> s + a - old

  extract = return

-- | Window Size. It computes the size of the sliding window.
windowSize :: MonadIO m => Fold m (Double, Maybe Double) Double 
windowSize = Fold.foldl' step initial

  where

  initial = 0 :: Double
  
  step w x@(a, ma) = 
    case ma of
      Nothing -> w + 1
      _ -> w

-- | Arithmetic mean. This uses Kahan-Babuska-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
{-# INLINE mean #-}
mean :: MonadIO m => Fold m (Double, Maybe Double) Double
mean = Fold.teeWith (/) sum windowSize

{-# INLINE powerSum #-}
powerSum :: MonadIO m => Int -> Fold m (Double, Maybe Double) Double
powerSum i = Fold.lmap (\(a, ma) -> (a ^ i, (^i) <$> ma)) sum

{-# INLINE powerSumAvg #-}
powerSumAvg :: MonadIO m => Int -> Fold m (Double, Maybe Double) Double
-- powerSumAvg ws@(Finite w) i =
--   Fold.teeWith (/) (powerSum ws i)
--   (fmap (fromIntegral . P.min w) Fold.length)
powerSumAvg i =
  Fold.teeWith (/) (powerSum i)
  (fmap fromIntegral Fold.length)

{-# INLINE variance #-}
variance :: MonadIO m => Fold m (Double, Maybe Double) Double
variance = Fold.teeWith (\p2 m -> p2 - m ^ 2) (powerSumAvg 2) mean

{-# INLINE stdDev #-}
stdDev :: MonadIO m => Fold m (Double, Maybe Double) Double
stdDev = sqrt <$> variance

{-# INLINE stdErrMean #-}
stdErrMean :: MonadIO m => Int -> Fold m (Double, Maybe Double) Double
stdErrMean i =
  Fold.teeWith (\sd n -> sd / (sqrt . fromIntegral) n) stdDev
  (fmap fromIntegral Fold.length)

{-# INLINE skewness #-}
skewness :: MonadIO m => Fold m (Double, Maybe Double) Double
skewness =
  toFold $
  (\p3 sd m -> p3 / sd ^ 3 - 3 * (m / sd) - (m / sd) ^ 3)
  <$> Tee (powerSumAvg 3)
  <*> Tee stdDev
  <*> Tee mean

{-# INLINE kurtosis #-}
kurtosis :: MonadIO m => Fold m (Double, Maybe Double) Double
kurtosis =
  toFold $
  (\p4 p3 sd m ->
     p4 / sd ^ 4 - 4 * ((m / sd) * (p3 / sd ^ 3)) - 3 * ((m / sd) ^ 4))
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
welfordMean :: MonadIO m => Fold m (Double, Maybe Double) Double
welfordMean = Fold step initial extract

  where

  initial = return $ Partial $ Tuple3' (0 :: Double) (0 :: Double) (0 :: Double)

  step (Tuple3' x n w) (y, my) =
    let n' = n + 1
        w' = w + 1
    in
      return $
        case my of
          Nothing -> Partial $ Tuple3' (x + (y - x) / n') n' w'
          Just old -> Partial $ Tuple3' (x + (y - x) / w + (x - old) / w) n' w

  extract (Tuple3' x _ _) = return x

{-# INLINE geometricMean #-}
geometricMean :: MonadIO m => Fold m (Double, Maybe Double) Double
geometricMean = exp <$> Fold.lmap (bimap log (log <$>)) mean
