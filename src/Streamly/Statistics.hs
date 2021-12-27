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
sum :: MonadIO m => WindowSize -> Fold m Double Double
sum Infinite = Fold step initial extract

  where

  initial = return $ Partial $ Tuple' (0 :: Double) (0 :: Double)

  step (Tuple' s c) a =
    let y = a - c
        t = s + y
        c' = (t - s) - y
      in return $ Partial $ Tuple' t c'

  extract (Tuple' s _) = return s

sum (Finite w) = Fold step initial extract

  where

  initial =
    fmap (\(a, b) -> Partial $ Tuple5' a b (0 :: Int) (0 :: Double) (0 :: Double)) $
    liftIO $ Ring.new w

  step (Tuple5' rb rh i s c) a
    | i < w = do
      let y = a - c
          t = s + y
          c' = (t - s) - y
      rh1 <- liftIO $ Ring.unsafeInsert rb rh (a + c')
      return $ Partial $ Tuple5' rb rh1 (i + 1) t c'
    | otherwise = do
      a' <- liftIO $ peek rh
      let s' = s - a'
          y = a - c
          t = s' + y
          c' = (t - s') - y
      rh1 <- liftIO $ Ring.unsafeInsert rb rh (a + c')
      return $ Partial $ Tuple5' rb rh1 i t c'

  extract (Tuple5' _ _ _ t _) = return t

-- | Arithmetic mean. This uses Kahan-Babuska-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
{-# INLINE mean #-}
mean :: MonadIO m => WindowSize -> Fold m Double Double
mean ws@Infinite =
  Fold.teeWith (/) (sum ws) (fmap fromIntegral Fold.length)
mean ws@(Finite w) =
  Fold.teeWith (/) (sum ws) (fmap (fromIntegral . P.min w) Fold.length)

{-# INLINE powerSum #-}
powerSum :: MonadIO m => WindowSize -> Int -> Fold m Double Double
powerSum ws i = Fold.lmap (^ i) $ sum ws

{-# INLINE powerSumAvg #-}
powerSumAvg :: MonadIO m => WindowSize -> Int -> Fold m Double Double
powerSumAvg ws@(Finite w) i =
  Fold.teeWith (/) (powerSum ws i)
  (fmap (fromIntegral . P.min w) Fold.length)
powerSumAvg ws@Infinite i =
  Fold.teeWith (/) (powerSum ws i)
  (fmap fromIntegral Fold.length)

{-# INLINE variance #-}
variance :: MonadIO m => WindowSize -> Fold m Double Double
variance ws = Fold.teeWith (\p2 m -> p2 - m ^ 2) (powerSumAvg ws 2) (mean ws)

{-# INLINE stdDev #-}
stdDev :: MonadIO m => WindowSize -> Fold m Double Double
stdDev ws = sqrt <$> variance ws

{-# INLINE stdErrMean #-}
stdErrMean :: MonadIO m => WindowSize -> Int -> Fold m Double Double
stdErrMean ws@(Finite w) i =
  Fold.teeWith (\sd n -> sd / (sqrt . fromIntegral) n) (stdDev ws)
  (fmap (\x -> fromIntegral (P.min w x)) Fold.length)
stdErrMean ws@(Infinite) i =
  Fold.teeWith (\sd n -> sd / (sqrt . fromIntegral) n) (stdDev ws)
  (fmap (\x -> fromIntegral x) Fold.length)

{-# INLINE skewness #-}
skewness :: MonadIO m => WindowSize -> Fold m Double Double
skewness ws =
  toFold $
  (\p3 sd m -> p3 / sd ^ 3 - 3 * (m / sd) - (m / sd) ^ 3)
  <$> Tee (powerSumAvg ws 3)
  <*> Tee (stdDev ws)
  <*> Tee (mean ws)

{-# INLINE kurtosis #-}
kurtosis :: MonadIO m => WindowSize -> Fold m Double Double
kurtosis ws =
  toFold $
  (\p4 p3 sd m ->
     p4 / sd ^ 4 - 4 * ((m / sd) * (p3 / sd ^ 3)) - 3 * ((m / sd) ^ 4))
  <$> Tee (powerSumAvg ws 4)
  <*> Tee (powerSumAvg ws 3)
  <*> Tee (stdDev ws)
  <*> Tee (mean ws)

-- | Arithmetic mean. This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
--
-- Compared to 'mean', this loses a surprising amount of precision
-- unless the inputs are very large.
{-# INLINE welfordMean #-}
welfordMean :: MonadIO m => WindowSize -> Fold m Double Double
welfordMean Infinite = Fold step (return begin) (return . done)

  where

  begin = Partial $ Tuple' (0 :: Double) (0 :: Double)

  step (Tuple' x n) y =
    return $
    let n' = n + 1
      in Partial $ Tuple' (x + (y - x) / n') n'
  done (Tuple' x _) = x

welfordMean (Finite w') = Fold step initial extract

  where

  w = fromIntegral w'

  initial =
    fmap (\(a, b) -> Fold.Partial $ Tuple5' a b (0 :: Int) (0 :: Double) (0 :: Double)) $
    liftIO $ Ring.new w'

  step (Tuple5' rb rh i x n) y
    | i < w' = do
      rh1 <- liftIO $ Ring.unsafeInsert rb rh y
      let n' = n + 1
      return $ Partial $ Tuple5' rb rh1 (i + 1) (x + (y - x) / n') n'
    | otherwise = do
      a' <- liftIO $ peek rh
      rh1 <- liftIO $ Ring.unsafeInsert rb rh y
      return $ Partial $ Tuple5' rb rh1 (i + 1) (x + (y - x) / w + (x - a') / w) n

  extract (Tuple5' _ _ _ t _) = return t

{-# INLINE geometricMean #-}
geometricMean :: MonadIO m => WindowSize -> Fold m Double Double
geometricMean ws = exp <$> Fold.lmap log (mean ws)
