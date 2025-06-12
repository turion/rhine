module FRP.Rhine.Bayes where

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population

-- dunai
import qualified Control.Monad.Trans.MSF.Reader as DunaiReader

-- dunai-bayes
import qualified Data.MonadicStreamFunction.Bayes as DunaiBayes

-- rhine
import FRP.Rhine

-- * Inference methods

-- | Run the Sequential Monte Carlo algorithm continuously on a 'ClSF'.
runPopulationCl ::
  forall m cl a b.
  Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler (see 'Control.Monad.Bayes.Population' for some standard choices)
  (forall x. Population m x -> Population m x) ->
  -- | A signal function modelling the stochastic process on which to perform inference.
  --   @a@ represents observations upon which the model should condition, using e.g. 'score'.
  --   It can also additionally contain hyperparameters.
  --   @b@ is the type of estimated current state.
  ClSF (Population m) cl a b ->
  ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = DunaiReader.readerS . DunaiBayes.runPopulationS nParticles resampler . DunaiReader.runReaderS

-- * Short standard library of stochastic processes

-- | White noise, that is, an independent normal distribution at every time step.
whiteNoise :: MonadDistribution m => Double -> Behaviour m td Double
whiteNoise sigma = constMCl $ normal 0 sigma

-- | Construct a Lévy process from the increment between time steps.
levy ::
  (MonadDistribution m, VectorSpace v (Diff td)) =>
  -- | The increment function at every time step. The argument is the difference between times.
  (Diff td -> m v) ->
  Behaviour m td v
levy incrementor = sinceLastS >>> arrMCl incrementor >>> sumS

-- | The Wiener process, also known as Brownian motion.
wiener
  , brownianMotion ::
    (MonadDistribution m, Diff td ~ Double) =>
    -- | Time scale of variance.
    Diff td ->
    Behaviour m td Double
wiener timescale = levy $ \diffTime -> normal 0 $ sqrt $ diffTime / timescale
brownianMotion = wiener

-- | The Wiener process, also known as Brownian motion, with varying variance parameter.
wienerVarying
  , brownianMotionVarying ::
    (MonadDistribution m, Diff td ~ Double) =>
    BehaviourF m td (Diff td) Double
wienerVarying = proc timeScale -> do
  diffTime <- sinceLastS -< ()
  let stdDev = sqrt $ diffTime / timeScale
  increment <-
    if stdDev > 0
      then arrM $ normal 0 -< stdDev
      else returnA -< 0
  sumS -< increment
brownianMotionVarying = wienerVarying

-- | The 'wiener' process transformed to the Log domain, also called the geometric Wiener process.
wienerLogDomain ::
  (MonadDistribution m, Diff td ~ Double) =>
  -- | Time scale of variance
  Diff td ->
  Behaviour m td (Log Double)
wienerLogDomain timescale = wiener timescale >>> arr Exp

-- | See 'wienerLogDomain' and 'wienerVarying'.
wienerVaryingLogDomain ::
  (MonadDistribution m, Diff td ~ Double) =>
  BehaviourF m td (Diff td) (Log Double)
wienerVaryingLogDomain = wienerVarying >>> arr Exp
