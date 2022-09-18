module FRP.Rhine.Bayes where

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Population
import Control.Monad.Bayes.Weighted hiding (flatten)

-- dunai
import qualified Control.Monad.Trans.MSF.Reader as DunaiReader

-- dunai
import Data.MonadicStreamFunction.Bayes (SoftEq)
import qualified Data.MonadicStreamFunction.Bayes as DunaiBayes

-- rhine
import FRP.Rhine

-- FIXME Does this haddock work?
-- | See 'Dunai.bayesFilter''
bayesFilter' :: (MonadInfer m, SoftEq sensor) =>
  -- | model
  ClSF m cl input (sensor, state) ->
  -- | external sensor, data source
  ClSF m cl input sensor ->
  ClSF m cl input (sensor, state)
bayesFilter' = DunaiBayes.bayesFilter'

-- FIXME Does this haddock work?
-- | See 'Dunai.bayesFilter'
bayesFilter :: (MonadInfer m, SoftEq sensor) =>
  ClSF m cl input (sensor, latent) ->
  -- | external sensor, data source
  ClSF m cl (input, sensor) latent
bayesFilter = DunaiBayes.bayesFilter

runPopulationCl :: forall m cl a b . Monad m =>
  -- | Number of particles
  Int ->
  -- | Resampler
  (forall x . Population m x -> Population m x)
  -> ClSF (Population m) cl a b
  -> ClSF m cl a [(b, Log Double)]
runPopulationCl nParticles resampler = DunaiReader.readerS . DunaiBayes.runPopulationS nParticles resampler . DunaiReader.runReaderS

collapseCl :: MonadInfer m => ClSF (Population m) cl a b -> ClSF m cl a b
collapseCl = hoistClSF collapse

-- FIXME unit test. Does this what I think it does?
properCl :: MonadSample m => ClSF (Population m) cl a b -> ClSF (Weighted m) cl a b
properCl = hoistClSF proper

-- * Short standard library of stochastic processes

-- FIXME multivariate version
-- | White noise, that is, an independent normal distribution at every time step
whiteNoise :: MonadSample m => Double -> Behaviour m td Double
whiteNoise sigma = constMCl $ normal 0 sigma

-- | Construct a Lévy process from the increment between time steps
levy ::
  (MonadSample m, VectorSpace v (Diff td)) =>
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
-- For now it is 1-d.
-- wiener :: VectorSpaceWithBasis v (Diff td) =>
wiener ::
  (MonadSample m, Diff td ~ Double) =>
  -- | Time scale of variance
  Diff td ->
    -- FIXME Do I need a further parameter
  Behaviour m td Double
wiener timescale = levy $ \diffTime -> normal 0 $ diffTime / timescale
