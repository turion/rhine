module Data.MonadicStreamFunction.Bayes where

-- base
import Control.Arrow
import Data.Functor (($>))
import Data.Tuple (swap)

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
resampleMoveSequentialMonteCarlo nParticles nMC resampler = go . (spawn nParticles $>)
  where
    go ::
      Monad m =>
      Population m (MSF (Static.Traced (Population m)) a b) ->
      -- Population m (MSF (t (Population m)) a b) ->
      MSF m a [(b, Log Double)]
    go msfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- FIXME This normalizes, which introduces bias, whatever that means
      bAndMSFs <- runPopulation
        $ normalize
        $ Debug.Trace.trace "4"
        $ marginal
        $ Debug.Trace.trace "3"
        $ composeCopies nMC (Debug.Trace.trace "mhStep" mhStep)
        $ Debug.Trace.trace "2"
        $ Control.Monad.Bayes.Traced.hoist (Debug.Trace.trace "resampler" resampler)
        $ Debug.Trace.trace "1"
        $ flip unMSF (Debug.Trace.trace "a" a) =<< lift (tracePop "msfs" msfs)
      return $
        second (go . fromWeightedList . return) $
          unzip $
            (swap . fmap fst &&& swap . fmap snd) . swap <$> Debug.Trace.trace "bAndMSFs" bAndMSFs

-- | Apply a function a given number of times.
composeCopies :: Int -> (a -> a) -> (a -> a)
composeCopies k f = foldr (.) id (replicate k f)

tracePop :: Monad m => String -> Population m a -> Population m a
tracePop msg = fromWeightedList . fmap (\pop -> Debug.Trace.traceShow (msg, length pop) pop) . runPopulation

-- resampleMoveSequentialMonteCarlo nParticles nMC resampler = morphS marginal $ runPopulationS nParticles $ freeze . composeCopies nMC mhStep . hoistTrace resampler

-- FIXME see PR re-adding this to monad-bayes
normalize :: Monad m => Population m a -> Population m a
normalize = fromWeightedList . fmap (\particles -> Debug.Trace.traceShow ("length particles", length particles) $ second (/ (sum $ snd <$> particles)) <$> particles) . runPopulation
