module FRP.Rhine.Bayes where

-- transformers
import Control.Monad.Trans.Reader (ReaderT(..))

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
import Control.Monad.Trans.MSF (MSFExcept)
import Control.Monad.Trans.Class (MonadTrans (lift))
import qualified Control.Monad.Trans.MSF.Except as Dunai

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

-- | Inhomogeneous Poisson point process, as described in:
--   https://en.wikipedia.org/wiki/Poisson_point_process#Inhomogeneous_Poisson_point_process
--
--   * The input is the inverse of the current rate or intensity.
--     It corresponds to the average duration between two events.
--   * The output is the number of events since the last tick.
poissonInhomogeneous ::
  (MonadDistribution m, Real (Diff td), Fractional (Diff td)) =>
  BehaviourF m td (Diff td) Int
poissonInhomogeneous = arrM $ \rate -> ReaderT $ \diffTime -> poisson $ realToFrac $ sinceLast diffTime / rate

-- | Like 'poissonInhomogeneous', but the rate is constant.
poissonHomogeneous ::
  (MonadDistribution m, Real (Diff td), Fractional (Diff td)) =>
  Diff td ->
    -- ^ The (constant) rate of the process
  BehaviourF m td () Int
poissonHomogeneous rate = arr (const rate) >>> poissonInhomogeneous

-- | The Gamma process, https://en.wikipedia.org/wiki/Gamma_process.
--
--   The live input corresponds to inverse shape parameter, which is variance over mean.
gammaInhomogeneous ::
  (MonadDistribution m, Real (Diff td), Fractional (Diff td), Floating (Diff td)) =>
  Diff td ->
  -- ^ The scale parameter
  BehaviourF m td (Diff td) Int
gammaInhomogeneous gamma = proc rate -> do
  t <- sinceInitS -< ()
  accumulateWith (+) 0 <<< poissonInhomogeneous -< gamma / t * exp (- t / rate)

-- | The inhomogeneous Bernoulli process, https://en.wikipedia.org/wiki/Bernoulli_process
--
--   Throws a coin to a given probability at each tick.
--   The live input is the probability.
bernoulliInhomogeneous :: MonadDistribution m => BehaviourF m td Double Bool
bernoulliInhomogeneous = arrMCl bernoulli

instance (MonadDistribution m) => MonadDistribution (MSFExcept m a b) where
  random = Dunai.once_ random
