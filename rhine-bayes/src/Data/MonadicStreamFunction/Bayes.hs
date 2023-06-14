{-# LANGUAGE NamedFieldPuns #-}
module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Control.Monad ( (<=<) )
import Data.Functor (($>))
import Data.Tuple (swap)

-- transformers

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Population

-- dunai
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))
import Control.Monad.Trans.MSF.List (widthFirst)
import Control.Monad.Trans.List (ListT(..))
import Control.Monad.Trans.MSF (performOnFirstSample)
import Data.Bifunctor (bimap)
import Control.DeepSeq (NFData1 (liftRnf))
import Control.Monad.Bayes.Weighted (weighted)

-- | Run the Sequential Monte Carlo algorithm continuously on an 'MSF'
runPopulationS ::
  forall m a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  MSF (Population m) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
runPopulationS nParticles resampler = runPopulationsS resampler . (spawn nParticles $>)
{-# INLINE runPopulationS #-}

-- | Run the Sequential Monte Carlo algorithm continuously on a 'Population' of 'MSF's
runPopulationsS ::
  Monad m =>
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  Population m (MSF (Population m) a b) ->
  MSF m a [(b, Log Double)]
runPopulationsS resampler = go
  where
    go msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      bAndMSFs <- runPopulation $ normalize $ resampler $ flip unMSF a =<< msfs
      return $
        first forceParticles $
        second (go . fromWeightedList . return . forceParticles) $
          unzip $
            (swap . fmap fst &&& swap . fmap snd) . swap <$> forceParticles bAndMSFs
    {-# INLINE go #-}
{-# INLINE runPopulationsS #-}

-- runPopulationS' ::
--   Monad m =>
--   Int ->
--   (forall x. Population m x -> Population m x) ->
--   MSF (Population m) a b ->
--   MSF m a [(b, Log Double)]
-- runPopulationS' nParticles resampler = morphS _ . widthFirst . morphGS (fmap (_ . ListT . _ . fmap (fmap (\((b, c), p) -> (((b, p), c), p))) . runPopulation . normalize . resampler)). performOnFirstSample . (spawn nParticles $>)
-- (\((b, c), p) -> ((b, p), c))

morphGG :: (Monad n, NFData1 t) => (forall c . (c, a1 -> c -> m (b1, c)) -> (t c, a2 -> t c -> n (b2, t c))) -> MSF m a1 b1 -> MSF n a2 b2
morphGG morph msf =
  -- let (state, transition) = morph (msf, flip unMSF) in mkMSF (\a s -> liftRnf (`seq` ()) s `seq` transition a s) state
  let (state, transition) = morph (msf, flip unMSF)
      go state' = MSF $ \a -> second go <$> (\a s -> liftRnf (`seq` ()) s `seq` transition a s) a state'
  in go state

-- morphGG' :: (Monad n, NFData1 t) => (forall c . (c, a1 -> c -> m (b1, c)) -> (t c, a2 -> t c -> n (b2, t c))) -> MSF m a1 b1 -> MSF n a2 b2
-- morphGG' morph msf = MSF $ \a2 ->

mkMSF :: Functor m => (a -> s -> m (b, s)) -> s -> MSF m a b
mkMSF transition = go
  where
    go state = MSF $ \a -> second go <$> transition a state
{-# INLINE  mkMSF #-}

runPopulationS' ::
  Monad m =>
  Int ->
  (forall x. Population m x -> Population m x) ->
  MSF (Population m) a b ->
  MSF m a [(b, Log Double)]
runPopulationS' nParticles resampler = morphGG (\(state, step) -> (WeightedList [(state, 1)], \a -> fmap (bimap forceParticles (WeightedList . forceParticles) . unzip . map (\((b, c), p) -> ((b, p), (c, p))) . forceParticles) . runPopulation . normalize . resampler . (step a <=< toPopulation))) . performOnFirstSample . (spawn nParticles $>)

newtype WeightedList a = WeightedList { unWeightedList :: [(a, Log Double)]}

instance NFData1 WeightedList where
  liftRnf forceA WeightedList { unWeightedList } = liftRnf (\(a, p) -> p `seq` forceA a `seq` ()) unWeightedList

toPopulation :: Monad m => WeightedList a -> Population m a
toPopulation = fromWeightedList . return . unWeightedList

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap normalizeParticles . runPopulation
{-# INLINE normalize #-}

normalizeParticles :: [(b, Log Double)] -> [(b, Log Double)]
normalizeParticles particles = second (/ totalWeight particles) <$> particles
{-# NOINLINE normalizeParticles #-}

totalWeight :: [(b, Log Double)] -> Log Double
totalWeight particles = sum $ snd <$> particles
{-# NOINLINE totalWeight #-}

forceParticles :: [(b, Log Double)] -> [(b, Log Double)]
forceParticles [] = []
forceParticles ((b, p) : particles) = p `seq` b `seq` (b, p) : forceParticles particles
