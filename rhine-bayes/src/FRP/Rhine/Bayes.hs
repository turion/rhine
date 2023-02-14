module FRP.Rhine.Bayes where

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population

-- automaton
import qualified Data.Automaton.Trans.Reader as AutomatonReader

-- rhine-bayes
import qualified Data.Automaton.Bayes as AutomatonBayes
-- FIXME Need to excavate these from dunai-bayes? Or do we never need this?
import Data.MonadicStreamFunction.Bayes (SoftEq, similarity)

-- rhine
import FRP.Rhine

bayesFilter' ::
  (MonadMeasure m, SoftEq sensor) =>
  -- | model
  ClSF m cl input (sensor, state) ->
  -- | external sensor, data source
  ClSF m cl input sensor ->
  ClSF m cl input (sensor, state)
bayesFilter' model sensor = proc input -> do
  output <- sensor -< input
  estimatedState <- bayesFilter model -< (input, output)
  returnA -< (output, estimatedState)

{- | Condition on one output of a distribution.

   p(x,y | theta) ~> p(x | y, theta)
-}
bayesFilter ::
  (MonadMeasure m, SoftEq sensor) =>
  ClSF m cl input (sensor, latent) ->
  -- | external sensor, data source
  ClSF m cl (input, sensor) latent
bayesFilter model = proc (input, measuredOutput) -> do
  (estimatedOutput, estimatedState) <- model -< input
  arrM score -< similarity estimatedOutput measuredOutput
  returnA -< estimatedState

-- * Inference methods

-- | Run the Sequential Monte Carlo algorithm continuously on a 'ClSF'.
runPopulationCl ::
  forall m cl a b.
  (Monad m, MonadDistribution m) =>
  -- | Number of particles
  Int ->
  -- | Resampler (see 'Control.Monad.Bayes.PopulationT' for some standard choices)
  (forall x m. (MonadDistribution m) => PopulationT m x -> PopulationT m x) ->
  -- | A signal function modelling the stochastic process on which to perform inference.
  --   @a@ represents observations upon which the model should condition, using e.g. 'score'.
  --   It can also additionally contain hyperparameters.
  --   @b@ is the type of estimated current state.
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = AutomatonReader.readerS . AutomatonBayes.runPopulationS nParticles resampler . AutomatonReader.runReaderS

-- FIXME We could simply not output the param here, then the user can decide to do that or not in b
runPopulationParamSimple :: Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  m param ->
  ClSF (Population m) cl (param, a) b ->
  -- FIXME Why not ClSF m a (Population b)
  ClSF m cl a [((b, param), Log Double)]
runPopulationParamSimple nParticles resampler param = DunaiReader.readerS . DunaiBayes.runPopulationParamSimpleS nParticles resampler param . (arr (\(param, (timeInfo, a)) -> (timeInfo, (param, a))) >>>) . DunaiReader.runReaderS

runPopulationParamDirichletConstant ::
  (Monad m, Real (Diff (Time cl)), Fractional (Diff (Time cl))) =>
  -- | Number of particles
  Int ->
  -- | Dirichlet time constant: The time interval after which you require a good estimate of your parameter
  Diff (Time cl) ->
  -- | Initial time: A small time above 0 that we can consider passed at the first sample
  Diff (Time cl) ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  m param ->
  ClSF (Population m) cl (param, a) b ->
  -- FIXME Why not MSF m a (Population b)
  ClSF m cl a [((b, param), Log Double)]
runPopulationParamDirichletConstant nParticles t t0 resampler param clsf = proc a -> do
  tCurrent <- sinceInitS -< ()
  let pNew = realToFrac $ t / (tCurrent + t0)
  DunaiReader.readerS $ (<<< arr assoc2) $ DunaiBayes.runPopulationParamDirichletConstant nParticles resampler param $ (<<< arr assoc) $ DunaiReader.runReaderS clsf -< (pNew, a)

-- FIXME I shouldn't need nParticles. When I sample from the prior predictive, I should be able to start with 0 basically and let t0 manage how many I sample on the first step
runPopulationParamDirichletElastic ::
  (Monad m, Real (Diff (Time cl)), Fractional (Diff (Time cl)), RealFrac (Diff (Time cl))) =>
  -- | Number of particles after time constant
  Int ->
  -- | Dirichlet time constant: The time interval after which you require a good estimate of your parameter
  Diff (Time cl) ->
  -- | Initial time: A small time above 0 that we can consider passed at the first sample
  Diff (Time cl) ->
  -- | Resampler
  (forall x. Population m x -> Population m x) ->
  m param ->
  ClSF (Population m) cl (param, a) b ->
  -- FIXME Why not MSF m a (Population b)
  ClSF m cl a [((b, param), Log Double)]
runPopulationParamDirichletElastic nParticles t t0 resampler param clsf = proc a -> do
  tCurrent <- sinceInitS -< ()
  -- let pNew = realToFrac $ t / (tCurrent + t0)
  pAccumulated <- arr getSum <<< mappendS -< Sum $ t / (tCurrent + t0)
  pLast <- iPre 0 -< pAccumulated
  let nNew = floor (fromIntegral nParticles * pAccumulated) - floor (fromIntegral nParticles * pLast)
  DunaiReader.readerS $ (<<< arr assoc2) $ DunaiBayes.runPopulationParamDirichletElastic nParticles resampler param $ (<<< arr assoc) $ DunaiReader.runReaderS clsf -< (nNew, a)

assoc :: (param, (TimeInfo cl, a)) -> (TimeInfo cl, (param, a))
assoc (param, (timeInfo, a)) = (timeInfo, (param, a))

assoc2 :: (TimeInfo cl, (p, a)) -> (p, (TimeInfo cl, a))
assoc2 (timeInfo, (p, a)) = (p, (timeInfo, a))

collapseCl :: MonadMeasure m => ClSF (Population m) cl a b -> ClSF m cl a b
collapseCl = hoistClSF collapse

-- FIXME unit test. Does this what I think it does?
properCl :: MonadDistribution m => ClSF (Population m) cl a b -> ClSF (Weighted m) cl a b
properCl = hoistClSF proper

-- * Short standard library of stochastic processes

-- | A stochastic process is a behaviour that uses, as only effect, random sampling.
type StochasticProcess time a = forall m. (MonadDistribution m) => Behaviour m time a

-- | Like 'StochasticProcess', but with a live input.
type StochasticProcessF time a b = forall m. (MonadDistribution m) => BehaviourF m time a b

-- | White noise, that is, an independent normal distribution at every time step.
whiteNoise :: Double -> StochasticProcess td Double
whiteNoise sigma = constMCl $ normal 0 sigma

-- | Like 'whiteNoise', that is, an independent normal distribution at every time step.
whiteNoiseVarying :: StochasticProcessF td Double Double
whiteNoiseVarying = arrMCl $ normal 0

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
    (Diff td ~ Double) =>
    StochasticProcessF td (Diff td) Double
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
  (Diff td ~ Double) =>
  -- | Time scale of variance
  Diff td ->
  StochasticProcess td (Log Double)
wienerLogDomain timescale = wiener timescale >>> arr Exp

-- | See 'wienerLogDomain' and 'wienerVarying'.
wienerVaryingLogDomain ::
  (Diff td ~ Double) =>
  StochasticProcessF td (Diff td) (Log Double)
wienerVaryingLogDomain = wienerVarying >>> arr Exp

{- | Inhomogeneous Poisson point process, as described in:
  https://en.wikipedia.org/wiki/Poisson_point_process#Inhomogeneous_Poisson_point_process

  * The input is the inverse of the current rate or intensity.
    It corresponds to the average duration between two events.
  * The output is the number of events since the last tick.
-}
poissonInhomogeneous ::
  (MonadDistribution m, Real (Diff td), Fractional (Diff td)) =>
  BehaviourF m td (Diff td) Int
poissonInhomogeneous = arrM $ \rate -> ReaderT $ \timeInfo -> poisson $ realToFrac $ sinceLast timeInfo / rate

-- | Like 'poissonInhomogeneous', but the rate is constant.
poissonHomogeneous ::
  (MonadDistribution m, Real (Diff td), Fractional (Diff td)) =>
  -- | The (constant) rate of the process
  Diff td ->
  BehaviourF m td () Int
poissonHomogeneous rate = arr (const rate) >>> poissonInhomogeneous

{- | The Gamma process, https://en.wikipedia.org/wiki/Gamma_process.

  The live input corresponds to inverse shape parameter, which is variance over mean.
-}
gammaInhomogeneous ::
  (MonadDistribution m, Real (Diff td), Fractional (Diff td), Floating (Diff td)) =>
  -- | The scale parameter
  Diff td ->
  BehaviourF m td (Diff td) Int
gammaInhomogeneous gamma = proc rate -> do
  t <- sinceInitS -< ()
  accumulateWith (+) 0 <<< poissonInhomogeneous -< gamma / t * exp (-t / rate)

{- | The inhomogeneous Bernoulli process, https://en.wikipedia.org/wiki/Bernoulli_process

  Throws a coin to a given probability at each tick.
  The live input is the probability.
-}
bernoulliInhomogeneous :: (MonadDistribution m) => BehaviourF m td Double Bool
bernoulliInhomogeneous = arrMCl bernoulli
