-- base
import GHC.Float (float2Double, double2Float)

-- transformers
import Control.Monad.Trans.Class

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class
-- FIXME They should implement MMorph
import Control.Monad.Bayes.Population hiding (hoist)
import Control.Monad.Bayes.Sampler

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.IO

-- rhine-bayes
import FRP.Rhine.Bayes
import FRP.Rhine.Gloss.Common

type StdDev = Double
type Pos = (Double, Double)
type Sensor = Pos

-- | Harmonic oscillator with white noise
prior1d :: (MonadSample m, Diff td ~ Double) =>
  -- | Starting position
  Double ->
  -- | Starting velocity
  Double ->
  BehaviourF m td StdDev Double
prior1d initialPosition initialVelocity = feedback 0 $ proc (stdDev, position') -> do
  impulse <- arrM (normal 0) -< stdDev
  -- FIXME make -3 input, sample once at the beginning, or on every key stroke
  let acceleration = (-3) * position' + impulse
  -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
  velocity <- arr (+ initialPosition) <<< decayIntegral 100 -< acceleration
  position <- integralFrom initialVelocity -< velocity
  returnA -< (position, position)

-- | 2D harmonic oscillator with noise
prior :: (MonadSample m, Diff td ~ Double) => BehaviourF m td StdDev Pos
prior = prior1d 10 0 &&& prior1d 0 10

double2FloatTuple :: (Double, Double) -> (Float, Float)
double2FloatTuple = double2Float *** double2Float

decayIntegral :: (VectorSpace v (Diff td), Monad m) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

noise :: MonadSample m => Behaviour m td Pos
noise = let stdDev = 5 in whiteNoise stdDev &&& whiteNoise stdDev

generativeModel :: (MonadSample m, Diff td ~ Double) => BehaviourF m td Pos Sensor
generativeModel = proc latent -> do
  noiseNow <- noise -< ()
  returnA -< latent ^+^ noiseNow

-- FIXME make parameters like n particles/spawn, softeq interactive!
-- That means it'll not be a ClSF anymore

model :: (MonadSample m, Diff td ~ Double) => BehaviourF m td StdDev (Sensor, Pos)
model = proc stdDev -> do
  latent <- prior -< stdDev
  sensor <- generativeModel -< latent
  returnA -< (sensor, latent)

posterior :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td (StdDev, Sensor) Pos
posterior = bayesFilter model

-- FIXME Want ExceptT so we can exit with escape
type MySmallMonad = GlossConcT SamplerIO

data Result = Result
  { measured :: Sensor
  , latent :: Pos
  , particles :: [(Pos, Log Double)]
  }
  deriving Show

filtered :: Diff td ~ Double => BehaviourF MySmallMonad td StdDev Result
filtered = proc stdDev -> do
  (measuredPosition, actualPosition) <- model -< stdDev
  samples <- runPopulationCl 100 resampleSystematic posterior -< (stdDev, measuredPosition)
  returnA -< Result
    { measured = measuredPosition
    , latent = actualPosition
    , particles = samples
    }

-- TODO FPS counter so we can see how too many particles bog down performance.
-- Or rather decouple simulation and graphics, and then make simulation a busy loop (with a little sleep) and display the simulation rate.
visualisation :: BehaviourF MySmallMonad td Result ()
visualisation = proc Result { measured, latent, particles } -> do
  constMCl clearIO -< ()
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, green)
  drawParticles -< particles

drawBall :: BehaviourF MySmallMonad td (Pos, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl paintIO -< scale 20 20 $ uncurry translate (double2FloatTuple position) $ color theColor $ circleSolid $ double2Float width

drawParticle :: BehaviourF MySmallMonad td (Pos, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) violet)

drawParticles :: BehaviourF MySmallMonad td [(Pos, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

mainClSF :: Diff td ~ Double => BehaviourF MySmallMonad td () ()
mainClSF = proc () -> do
  let stdDev = 5
  output <- filtered -< stdDev
  visualisation -< output

-- FIXME should be in monad-bayes
-- In fact there should be a newtype for lifting MonadSample along a transformer,
-- and then all instances derived via it
instance MonadSample m => MonadSample (ExceptT e m) where
  random = lift random

-- See, this is why we need effect frameworks.
-- Or why monad-bayes needs newtypes for monad transformers
instance MonadSample m => MonadSample (GlossConcT m) where
  random = lift random
  -- FIXME Other PDs?

glossClock :: RescaledClock GlossSimClockIO Double
glossClock = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = float2Double
  }

main = do
  -- TODO would like push
  thing <- sampleIO
    $ launchGlossThread defaultSettings
      { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
    $ reactimateCl glossClock mainClSF
  print thing
