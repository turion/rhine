{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Rhine.Bayes where

-- transformers
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader (mapReaderT, ask, runReaderT)

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted hiding (flatten)

-- dunai
import Data.MonadicStreamFunction.InternalCore (MSF(..))

-- rhine
-- rhine
import FRP.Rhine hiding (normalize)
import Control.Monad (join)
import Data.Functor (($>))

import qualified Debug.Trace as Trace

bayesFilter' :: (MonadInfer m, Eq sensor) =>
  -- | model
  ClSF m cl input (sensor, state) ->
  -- | external sensor, data source
  ClSF m cl input sensor ->
  ClSF m cl input (sensor, state)
bayesFilter' model sensor = proc input -> do
  output <- sensor -< input
  estimatedState <- bayesFilter model -< (input, output)
  returnA -< (output, estimatedState)

bayesFilter :: (MonadInfer m, Eq sensor) =>
  ClSF m cl input (sensor, latent) ->
  -- | external sensor, data source
  ClSF m cl (input, sensor) latent
bayesFilter model = proc (input, measuredOutput) -> do
  (estimatedOutput, estimatedState) <- model -< input
  arrM condition -< estimatedOutput == measuredOutput
  returnA -< estimatedState

runPopulationCl :: forall m cl a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler clsf = runPopulationCl' $ spawn nParticles $> clsf
  where
    runPopulationCl' :: Monad m => Population m (ClSF (Population m) cl a b) -> ClSF m cl a [(b, Log Double)]
    runPopulationCl' clsfs = MSF $ \a -> do
      -- TODO This is quite different than the dunai version now. Maybe it's right nevertheless.
      -- funsies <- lift $ runPopulation clsfs
      timeInfo <- ask
      let thing = flip runReaderT timeInfo . flip unMSF a <$> clsfs
      -- funsies2 <- lift $ runPopulation thing
      -- funsies3 <- lift $ mapM runPopulation $ fst <$> funsies2
      thang <- lift $ runPopulation $ join $
        -- tracePopWeights "funsies" funsies `seq` tracePopWeights "funsies2" funsies2 `seq` tracePopWeights "funsies3" (concat funsies3) `seq`
        thing
      -- FIXME This abominal lambda could be done away by using Weighted?
      let (currentPopulation, continuations) = unzip $ (\((b, clsf), weight) -> ((b, weight), (clsf, weight))) <$> thang
      -- FIXME This normalizes, which introduces bias, whatever that means
      return (currentPopulation, runPopulationCl' $ normalize $ resampler $ fromWeightedList $ return continuations)

-- tracePopWeights :: String -> [(a, Log Double)] -> [(a, Log Double)]
-- tracePopWeights msg as = Trace.trace (msg ++ show (map snd as)) as

collapseCl :: MonadInfer m => ClSF (Population m) cl a b -> ClSF m cl a b
collapseCl = hoistClSF collapse

-- FIXME unit test. Does this what I think it does?
properCl :: MonadSample m => ClSF (Population m) cl a b -> ClSF (Weighted m) cl a b
properCl = hoistClSF proper

-- FIXME separate module. actually, separate package
class Statistical a where
  statistic :: [(a, Double)] -> a

newtype Average a = Average { getAverage :: a }
  deriving (Num, Fractional)

instance Fractional a => Statistical (Average a) where
  -- FIXME realToFrac isn't nice, but unfortunately we're stuck with Doubles
  statistic = sum . fmap (uncurry (*) . second realToFrac)

-- FIXME naming: clashes with clsf average
-- FIXME try coerce
average :: Fractional a => [(a, Double)] -> a
average = getAverage . statistic . fmap (first Average)
