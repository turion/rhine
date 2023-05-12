module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Data.Functor (($>))
import Data.Tuple (swap)
import Debug.Trace

-- transformers

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Traced

-- dunai
import Data.MonadicStreamFunction
import Data.MonadicStreamFunction.InternalCore (MSF (..))
import Control.Monad.Trans.Class
import Control.Monad.Bayes.Class (MonadDistribution)
import qualified Control.Monad.Bayes.Traced.Static as Static
import Control.Monad.Bayes.Sequential.Coroutine (hoistFirst)
import Control.Monad.Trans.MSF (performOnFirstSample)
import qualified Control.Monad.Bayes.Traced.Dynamic as Dynamic

-- FIXME rename to sequentialMonteCarlo or smc?
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
      second (go . fromWeightedList . return) $
        unzip $
          (swap . fmap fst &&& swap . fmap snd) . swap <$> bAndMSFs

resampleMoveSequentialMonteCarlo ::
  forall m a b.
  MonadDistribution m =>
  -- (MonadDistribution m, HasTraced t, MonadTrans t) =>
  -- | Number of particles
  Int ->
  -- | Number of MC steps
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  MSF (Static.Traced (Population m)) a b ->
  -- MSF (t (Population m)) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
resampleMoveSequentialMonteCarlo nParticles nMC resampler = go . Control.Monad.Bayes.Traced.hoist (spawn nParticles >>) . pure
  where
    go ::
      Monad m =>
      Static.Traced (Population m) (MSF (Static.Traced (Population m)) a b) ->
      -- Population m (MSF (t (Population m)) a b) ->
      MSF m a [(b, Log Double)]
    go msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      let bAndMSFs =  composeCopies nMC mhStep
            $ Control.Monad.Bayes.Traced.hoist resampler
            $ flip unMSF a =<< msfs
      bs <- runPopulation $ marginal $ fst <$> bAndMSFs
      return (bs, go $ snd <$> bAndMSFs)

resampleMoveSequentialMonteCarloDynamic ::
  forall m a b.
  MonadDistribution m =>
  -- (MonadDistribution m, HasTraced t, MonadTrans t) =>
  -- | Number of particles
  Int ->
  -- | Number of MC steps
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  MSF (Dynamic.Traced (Population m)) a b ->
  -- MSF (t (Population m)) a b ->
  -- FIXME Why not MSF m a (Population b)
  MSF m a [(b, Log Double)]
resampleMoveSequentialMonteCarloDynamic nParticles nMC resampler = go . Dynamic.hoist (spawn nParticles >>) . pure
  where
    go ::
      Monad m =>
      Dynamic.Traced (Population m) (MSF (Dynamic.Traced (Population m)) a b) ->
      -- Population m (MSF (t (Population m)) a b) ->
      MSF m a [(b, Log Double)]
    go msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      let bAndMSFs = Dynamic.freeze
            $ composeCopies nMC Dynamic.mhStep
            $ Dynamic.hoist resampler
            $ flip unMSF a =<< msfs
      bs <- runPopulation $ Dynamic.marginal $ fst <$> bAndMSFs
      return (bs, go $ snd <$> bAndMSFs)

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

tracePop :: Monad m => String -> Population m a -> Population m a
-- tracePop msg = fromWeightedList . fmap (\pop -> Debug.Trace.traceShow (msg, length pop) pop) . runPopulation
tracePop _ = id

-- resampleMoveSequentialMonteCarlo nParticles nMC resampler = morphS marginal $ runPopulationS nParticles $ freeze . composeCopies nMC mhStep . hoistTrace resampler

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation

-- FIXME See PR to monad-bayes


-- | Only use the given resampler when the effective sample size is below a certain threshold
onlyBelowEffectiveSampleSize ::
  MonadDistribution m =>
  -- | The threshold under which the effective sample size must fall before the resampler is used.
  --   For example, this may be half of the number of particles.
  Double ->
  -- | The resampler to user under the threshold
  (forall n . MonadDistribution n => Population n a -> Population n a) ->
  -- | The new resampler
  (Population m a -> Population m a)
onlyBelowEffectiveSampleSize threshold resampler pop = do
  (particles, ess) <- lift $ runWithEffectiveSampleSize pop
  let newPop = fromWeightedList $ pure particles
  -- This assumes that the resampler does not mutate the m effects, as it should
  if ess < threshold then resampler newPop else newPop

-- | Compute the effective sample size of a population from the weights.
--
--   See https://en.wikipedia.org/wiki/Design_effect#Effective_sample_size
runWithEffectiveSampleSize :: Functor m => Population m a -> m ([(a, Log Double)], Double)
runWithEffectiveSampleSize = fmap (id &&& (effectiveSampleSizeKish . map (exp . ln . snd))) . runPopulation
  where
    effectiveSampleSizeKish :: [Double] -> Double
    effectiveSampleSizeKish weights = square (sum weights) / sum (square <$> weights)
    square :: Double -> Double
    square x = x * x

measureESS :: Monad m => MSF (Population m) a b -> MSF (Population m) a (b, Double)
measureESS = morphGS $ fmap $ \pop -> fromWeightedList $ do
  (particles, ess) <- runWithEffectiveSampleSize pop
  pure $ map (first (first (, ess))) particles

withESS :: Monad m => Double -> MSF (Population m) (a, Double) b -> MSF (Population m) a b
withESS initESS = feedback initESS . measureESS
