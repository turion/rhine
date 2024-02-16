{-# LANGUAGE ScopedTypeVariables #-}
module FRP.Rhine.Bayes where

-- transformers
import Control.Monad.Trans.Reader (ReaderT (..))

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
import Data.MonadicStreamFunction.InternalCore (unMSF)
import Data.Coerce (coerce)
import Control.Monad (forM)
import Data.MonadicStreamFunction.Bayes (runPopulationS)

-- * Inference methods

-- | Run the Sequential Monte Carlo algorithm continuously on a 'ClSF'.
runPopulationCl ::
  forall m cl a b.
  (Monad m) =>
  -- | Number of particles
  Int ->
  -- | Resampler (see 'Control.Monad.Bayes.PopulationT' for some standard choices)
  (forall x. PopulationT m x -> PopulationT m x) ->
  -- | A signal function modelling the stochastic process on which to perform inference.
  --   @a@ represents observations upon which the model should condition, using e.g. 'score'.
  --   It can also additionally contain hyperparameters.
  --   @b@ is the type of estimated current state.
  ClSF (PopulationT m) cl a b ->
  ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = DunaiReader.readerS . DunaiBayes.runPopulationS nParticles resampler . DunaiReader.runReaderS

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
  Behaviour m td Int
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


inferenceBuffer :: forall clA clS time m s a . (time ~ Time clS, time ~ Time clA, Monad m, MonadDistribution m)
     => Int ->
      (forall n a . MonadDistribution n =>  PopulationT n a -> PopulationT n a) ->
         Behaviour m time s -> (s -> a -> Log Double) -> ResamplingBuffer m clA clS a [s]
inferenceBuffer nParticles resampler process likelihood = msfBuffer' $ runPopulationS nParticles resampler thing >>> arr (fmap fst)
 where
  processParClock :: ClSF m (ParallelClock clA clS) () s
  processParClock = process
  thing :: Monad m => MSF (PopulationT m) (Either (TimeInfo clS) (TimeInfo clA, a)) s
  thing = proc tia -> do
    s <- DunaiReader.runReaderS $ liftClSF processParClock -< (either (retag Right) (retag Left . fst) tia, ())
    case tia of
      Left _ -> returnA -< s
      Right (_, a) -> do
        arrM factor -< likelihood s a
        returnA -< s
-- inferenceBuffer nParticles process likelihood = go $ replicate nParticles process
--  where
--   stepToTime :: TimeInfo cl -> ClSF m cl () s -> m (s, ClSF m cl () s)
--   stepToTime ti clsf = second SomeBehaviour <$> runReaderT (unMSF (getSomeBehaviour clsf) ()) ti

--   -- Add resamplnig here
--   stepAllToTime :: Monad m => (forall n . MonadDistribution n =>  PopulationT n a -> PopulationT n a) -> TimeInfo cl -> [ClSF m cl () s] -> m [(s, ClSF m cl () s)]
--   stepAllToTime resampler ti = fmap _ . runPopulationT . resampler . fromWeightedList . fmap _ . mapM (stepToTime ti)

--   go :: [ClSF m (ParallelClock clA clB) () s] -> ResamplingBuffer m clA clS a s
--   go msfs = ResamplingBuffer
--     { put = \ti a -> do
--         stepped <- forM msfs $ \msf -> do
--           msf' <- stepToTime (retag Left ti) msf
--           _ -- factor each msf individually, resample all at the end
--         return $ go $ snd <$> stepped
--     , get = _
--     }
