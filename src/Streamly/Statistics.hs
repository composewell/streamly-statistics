{-# LANGUAGE FlexibleContexts #-}
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
  , ema
  , sma
  , obv
  ) where

import Streamly
import Streamly.Internal.Data.Fold.Types (Fold(..))
import Streamly.Internal.Data.Strict
import Control.Monad.IO.Class (MonadIO(..))
import Foreign.Storable (Storable(..), sizeOf)
import Foreign.Ptr (plusPtr)

import qualified Streamly.Prelude as S
import qualified Streamly.Memory.Ring as RB
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Internal.Data.Fold as FL
import qualified Foreign.Storable.Record as Store
import Data.Function ((&))

import qualified Deque.Strict as DQ

import Prelude hiding (sum, min, max)
import qualified Prelude as P
import Data.Maybe (fromMaybe)

instance (Storable a, Storable b) => Storable (a,b) where
   sizeOf    = Store.sizeOf storePair
   alignment = Store.alignment storePair
   peek      = Store.peek storePair
   poke      = Store.poke storePair

{-# INLINE storePair #-}
storePair ::
   (Storable a, Storable b) =>
   Store.Dictionary (a,b)
storePair =
   Store.run $
    (,)
    <$> Store.element fst
    <*> Store.element snd



-- XXX Make the following more numerically stable. Try to extend welfordMean method.
-- XXX     - stdDev
-- XXX     - skewness
-- XXX     - kurtosis
-- XXX References:
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

-- | Range. The difference between the largest and smallest elements of a sample.
{-# INLINE range #-}
range :: Monad m => WindowSize -> Fold m Double Double
range ws = (-) <$> max ws <*> min ws

-- | The minimum element in the sample.
{-# INLINE min #-}
min :: Monad m => WindowSize -> Fold m Double Double
min Infinite = Fold step initial extract
  where
    initial = return $ 1 / 0
    step ma a
      | a < ma = return a
      | otherwise = return ma
    extract = return
min (Finite w) = Fold step initial extract
  where
    initial = return $ Tuple' (0 :: Int) (mempty :: DQ.Deque (Int, Double))
    step (Tuple' i q) a =
      return $ Tuple' (i + 1) (headCheck i q & dqloop (i, a))
    {-# INLINE headCheck #-}
    headCheck i q =
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
    extract (Tuple' _ q) = return $ snd $ fromMaybe (0, 1 / 0) $ DQ.head q

-- | The maximum element in the sample.
{-# INLINE max #-}
max :: Monad m => WindowSize -> Fold m Double Double
max Infinite = Fold step initial extract
  where
    initial = return $ -1 / 0
    step ma a
      | a > ma = return a
      | otherwise = return ma
    extract = return
max (Finite w) = Fold step initial extract
  where
    initial = return $ Tuple' (0 :: Int) (mempty :: DQ.Deque (Int, Double))
    step (Tuple' i q) a =
      return $ Tuple' (i + 1) (headCheck i q & dqloop (i, a))
    {-# INLINE headCheck #-}
    headCheck i q =
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
    extract (Tuple' _ q) = return $ snd $ fromMaybe (0, -1 / 0) $ DQ.head q

-- | The sum of all the elements in the sample. This uses Kahan-Babuska-Neumaier
-- summation.
{-# INLINE sum #-}
sum :: MonadIO m => WindowSize -> Fold m Double Double
sum Infinite = Fold step initial extract
  where
    initial = return $ Tuple' (0 :: Double) (0 :: Double)
    step (Tuple' s c) a =
      let y = a - c
          t = s + y
          c' = (t - s) - y
       in return $ Tuple' t c'
    extract (Tuple' s _) = return s
sum (Finite w) = Fold step initial extract
  where
    initial =
      fmap (\(a, b) -> Tuple5' a b (0 :: Int) (0 :: Double) (0 :: Double)) $
      liftIO $ RB.new w
    step (Tuple5' rb rh i s c) a
      | i < w = do
        let y = a - c
            t = s + y
            c' = (t - s) - y
        rh1 <- liftIO $ RB.unsafeInsert rb rh (a + c')
        return $ Tuple5' rb rh1 (i + 1) t c'
      | otherwise = do
        a' <- liftIO $ peek rh
        let s' = s - a'
            y = a - c
            t = s' + y
            c' = (t - s') - y
        rh1 <- liftIO $ RB.unsafeInsert rb rh (a + c')
        return $ Tuple5' rb rh1 i t c'
    extract (Tuple5' _ _ _ t _) = return t

-- | Arithmetic mean. This uses Kahan-Babuska-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
{-# INLINE mean #-}
mean :: MonadIO m => WindowSize -> Fold m Double Double
mean ws@(Infinite) =
  (\a b -> a / b) <$> sum ws <*> fmap (\x -> fromIntegral x) FL.length
mean ws@(Finite w) =
  (\a b -> a / b) <$> sum ws <*> fmap (\x -> fromIntegral (P.min w x)) FL.length

{-# INLINE powerSum #-}
powerSum :: MonadIO m => WindowSize -> Int -> Fold m Double Double
powerSum ws i = FL.lmap (\x -> x ^ i) $ sum ws

{-# INLINE powerSumAvg #-}
powerSumAvg :: MonadIO m => WindowSize -> Int -> Fold m Double Double
powerSumAvg ws@(Finite w) i =
  (\x n -> x / n) <$> powerSum ws i <*>
  fmap (\x -> fromIntegral (P.min w x)) FL.length
powerSumAvg ws@(Infinite) i =
  (\x n -> x / n) <$> powerSum ws i <*> fmap (\x -> fromIntegral x) FL.length

{-# INLINE variance #-}
variance :: MonadIO m => WindowSize -> Fold m Double Double
variance ws = (\p2 m -> p2 - m ^ 2) <$> powerSumAvg ws 2 <*> mean ws

{-# INLINE stdDev #-}
stdDev :: MonadIO m => WindowSize -> Fold m Double Double
stdDev ws = sqrt <$> variance ws

{-# INLINE stdErrMean #-}
stdErrMean :: MonadIO m => WindowSize -> Int -> Fold m Double Double
stdErrMean ws@(Finite w) i =
  (\sd n -> sd / (sqrt . fromIntegral) n) <$> stdDev ws <*>
  fmap (\x -> fromIntegral (P.min w x)) FL.length
stdErrMean ws@(Infinite) i =
  (\sd n -> sd / (sqrt . fromIntegral) n) <$> stdDev ws <*>
  fmap (\x -> fromIntegral x) FL.length

{-# INLINE skewness #-}
skewness :: MonadIO m => WindowSize -> Fold m Double Double
skewness ws =
  (\p3 sd m -> p3 / sd ^ 3 - 3 * (m / sd) - (m / sd) ^ 3) <$> powerSumAvg ws 3 <*>
  stdDev ws <*>
  mean ws

{-# INLINE kurtosis #-}
kurtosis :: MonadIO m => WindowSize -> Fold m Double Double
kurtosis ws =
  (\p4 p3 sd m ->
     p4 / sd ^ 4 - 4 * ((m / sd) * (p3 / sd ^ 3)) - 3 * ((m / sd) ^ 4)) <$>
  powerSumAvg ws 4 <*>
  powerSumAvg ws 3 <*>
  stdDev ws <*>
  mean ws

-- | Arithmetic mean. This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
--
-- Compared to 'mean', this loses a surprising amount of precision
-- unless the inputs are very large.
{-# INLINE welfordMean #-}
welfordMean :: MonadIO m => WindowSize -> Fold m Double Double
welfordMean Infinite = Fold step (return begin) (return . done)
  where
    begin = Tuple' (0 :: Double) (0 :: Double)
    step (Tuple' x n) y =
      return $
      let n' = n + 1
       in Tuple' (x + (y - x) / n') n'
    done (Tuple' x _) = x
welfordMean (Finite w') = Fold step initial extract
  where
    w = fromIntegral w'
    initial =
      fmap (\(a, b) -> Tuple5' a b (0 :: Int) (0 :: Double) (0 :: Double)) $
      liftIO $ RB.new w'
    step (Tuple5' rb rh i x n) y
      | i < w' = do
        rh1 <- liftIO $ RB.unsafeInsert rb rh y
        let n' = n + 1
        return $ Tuple5' rb rh1 (i + 1) (x + (y - x) / n') n'
      | otherwise = do
        a' <- liftIO $ peek rh
        rh1 <- liftIO $ RB.unsafeInsert rb rh y
        return $ Tuple5' rb rh1 (i + 1) (x + (y - x) / w + (x - a') / w) n
    extract (Tuple5' _ _ _ t _) = return t

{-# INLINE geometricMean #-}
geometricMean :: MonadIO m => WindowSize -> Fold m Double Double
geometricMean ws = exp <$> FL.lmap log (mean ws)

{-# INLINE ema #-}
ema :: MonadIO m => WindowSize -> Int -> Fold m Double Double
ema Infinite n' = Fold step initial extract
    where
        n = fromIntegral n'
        a = 2 / (n + 1)
        initial = return $ Tuple' (0 :: Int) (Nothing :: Maybe Double)
        step (Tuple' i x') x =
            return $ Tuple' (i + 1) $
                case x' of
                    Nothing -> Just x
                    Just em -> Just ((1 - a) * em + a * x)
        extract (Tuple' i x') = return $ fromMaybe 0 x'
ema (Finite w') n' = Fold step initial extract
  where
    w = fromIntegral w'
    n = fromIntegral n'
    a = 2 / (n + 1)
    initial =
      fmap (\(a, b) -> Tuple4' a b (0 :: Int) (Nothing :: Maybe Double)) $
      liftIO $ RB.new w'
    step (Tuple4' rb rh i x') y
      | i < w' = do
        rh1 <- liftIO $ RB.unsafeInsert rb rh y
        return $ Tuple4' rb rh1 (i + 1) $
            case x' of
                Nothing -> Just y
                Just em -> Just ((1 - a) * em + a * y)

      | otherwise = do
        x <- liftIO $ peek rh
        let z = RB.unsafeFoldRing
                    (rh `plusPtr` (w * sizeOf (undefined :: Double)))
                    (\em x -> (1 - a) * em + a * x) x rb
        rh1 <- liftIO $ RB.unsafeInsert rb rh y
        return $ Tuple4' rb rh1 (i + 1) (Just z)
    extract (Tuple4' _ _ _ x') = return $ fromMaybe 0 x'

{-# INLINE sma #-}
sma :: MonadIO m => WindowSize -> Fold m Double Double
sma = welfordMean

{-# INLINE obv #-}
obv :: MonadIO m => WindowSize -> Fold m (Double, Double) Double
obv Infinite = Fold step initial extract
    where
        initial = return $ Tuple' (0 :: Double) (Nothing :: Maybe Double)
        step (Tuple' close' acc') (close, volume) = do
            liftIO $ print acc'
            return $ Tuple' close $
                case acc' of
                    Nothing -> Just 0
                    Just acc -> Just $
                        case compare close close' of
                            GT -> acc + volume
                            LT -> acc - volume
                            EQ -> 0
        extract (Tuple' _ acc) = return (fromMaybe 0 acc)
obv (Finite w') = Fold step initial extract
  where
    w = fromIntegral w'
    initial =
      fmap (\(a, b) -> Tuple5' a b (0 :: Int) (0 :: Double) (Nothing :: Maybe Double)) $
      liftIO $ RB.new w'
    step (Tuple5' rb rh i close' acc') (close, volume)
      | i < w' = do
        let (bl, acc) =
                case acc' of
                    Nothing -> (0, 0)
                    Just z -> case compare close close' of
                                GT -> (-volume, z + volume)
                                LT -> (volume, z - volume)
                                EQ -> (z, 0)
        rh1 <- liftIO $ RB.unsafeInsert rb rh bl
        return $ Tuple5' rb rh1 (i + 1) close (Just acc)

      | otherwise = do
        x <- liftIO $ peek (rh `plusPtr` sizeOf (undefined :: Double))
        let (bl, acc) =
                case acc' of
                    Nothing -> (0, 0)
                    Just y -> case compare close close' of
                                GT -> (-volume, z + volume)
                                LT -> (volume, z - volume)
                                EQ -> (z, 0)
                                where z = y + x
        rh1 <- liftIO $ RB.unsafeInsert rb rh bl
        return $ Tuple5' rb rh1 (i + 1) close (Just acc)
    extract (Tuple5' _ _ _ _ x') = return $ fromMaybe 0 x'


