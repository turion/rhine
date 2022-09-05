{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FRP.Rhine.Bayes where

-- transformers
import Control.Monad.Trans.Reader (mapReaderT)

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted

-- dunai
import Data.MonadicStreamFunction.InternalCore (MSF(..))

-- rhine
import FRP.Rhine

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
  ClSF m cl input (sensor, state) ->
  -- | external sensor, data source
  ClSF m cl (input, sensor) state
bayesFilter model = proc (input, measuredOutput) -> do
  (estimatedOutput, estimatedState) <- model -< input
  arrM condition -< estimatedOutput == measuredOutput
  returnA -< estimatedState

runPopulationCl :: Monad m => ClSF (Population m) cl a b -> ClSF m cl a [(b, Log Double)]
runPopulationCl clsf = runPopulationCl' [clsf]
  where
    runPopulationCl' :: Monad m => [ClSF (Population m) cl a b] -> ClSF m cl a [(b, Log Double)]
    runPopulationCl' clsfs = MSF $ \a -> do
      -- FIXME Should I add the weights to the recursive call?
      -- FIXME Should I resample here?
      (bs, clsfs') <- unzip . concat <$> mapM (mapReaderT (fmap (fmap (\((b, clsf), weight) -> ((b, weight), clsf))) . runPopulation) . flip unMSF a) clsfs
      return (bs, runPopulationCl' clsfs')

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
