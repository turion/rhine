module FRP.Rhine.Bayes where

-- base
import Data.Monoid (Sum(..))

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population

-- dunai
import qualified Control.Monad.Trans.MSF.Reader as DunaiReader

-- rhine
import FRP.Rhine

-- rhine-bayes
import Data.MonadicStreamFunction.Bayes

-- * Inference methods

-- | Run the Sequential Monte Carlo algorithm continuously on a 'ClSF'.
runPopulationCl :: forall m cl a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler (see 'Control.Monad.Bayes.Population' for some standard choices)
  (forall x . Population m x -> Population m x)
  -- | A signal function modelling the stochastic process on which to perform inference.
  --   @a@ represents observations upon which the model should condition, using e.g. 'score'.
  --   It can also additionally contain hyperparameters.
  --   @b@ is the type of estimated current state.
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = DunaiReader.readerS . runPopulationS nParticles resampler . DunaiReader.runReaderS

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
runPopulationParamSimple nParticles resampler param = DunaiReader.readerS . runPopulationParamSimpleS nParticles resampler param . (arr (\(param, (timeInfo, a)) -> (timeInfo, (param, a))) >>>) . DunaiReader.runReaderS

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
  DunaiReader.readerS $ (<<< arr assoc2) $ runPopulationParamDirichletConstantS nParticles resampler param $ (<<< arr assoc) $ DunaiReader.runReaderS clsf -< (pNew, a)

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
  DunaiReader.readerS $ (<<< arr assoc2) $ runPopulationParamDirichletElasticS nParticles resampler param $ (<<< arr assoc) $ DunaiReader.runReaderS clsf -< (nNew, a)

assoc :: (param, (TimeInfo cl, a)) -> (TimeInfo cl, (param, a))
assoc (param, (timeInfo, a)) = (timeInfo, (param, a))

assoc2 :: (TimeInfo cl, (p, a)) -> (p, (TimeInfo cl, a))
assoc2 (timeInfo, (p, a)) = (p, (timeInfo, a))

-- * Short standard library of stochastic processes

-- FIXME The dimension of this is square root of time! Is it possible to capture that in a type, related to time-domain?
-- FIXME multivariate version
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
-- | Implement that in a 'VectorSpace', every vector can be decomposed into a linear combination of basis vectors.
--
-- There is a theorem that every vector space has a basis, but it is not constructive.
-- Thus there is no algorithm that constructs a basis, so it has to be supplied.
class VectorSpace v s => VectorSpaceWithBasis v s where
  -- | A chosen set of basis vectors. Expected to be linearly independent, and covering the whole space.
  basis :: [v]

instance VectorSpaceWithBasis Double Double where
  basis = [1]

instance VectorSpaceWithBasis Float Float where
  basis = [1]

-- FIXME The superclass constraint is not implemented for whatever strange reason
-- instance (VectorSpaceWithBasis v1 s, VectorSpaceWithBasis v2 s) => VectorSpaceWithBasis (v1, v2) s where
  -- basis = (,) <$> basis <*> basis

-- FIXME time domain should have an addDiffTime method, then I could output Diff td here and still sum it up with levy
-- FIXME this is an endless source of bad puns and inuendo... still the best name? Or rename to brownianMotion?
-- FIXME Learn about variances and implement multidimensional version. VectorSpaceWithBasis probably needed for that.

-- | The Wiener process, also known as Brownian motion.
wiener, brownianMotion ::
  (MonadDistribution m, Diff td ~ Double) =>
  -- | Time scale of variance.
  Diff td ->
  Behaviour m td Double
wiener timescale = levy $ \diffTime -> normal 0 $ sqrt $ diffTime / timescale

brownianMotion = wiener

-- | The Wiener process, also known as Brownian motion, with varying variance parameter.
wienerVarying, brownianMotionVarying ::
  (MonadDistribution m, Diff td ~ Double) =>
  BehaviourF m td (Diff td) Double
wienerVarying = proc timeScale -> do
  diffTime <- sinceLastS -< ()
  let stdDev = sqrt $ diffTime / timeScale
  increment <- if stdDev > 0
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
