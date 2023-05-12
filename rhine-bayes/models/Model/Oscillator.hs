-- | The model for the main example.
--
--   See rhine-bayes/app/Main.hs for details.
module Model.Oscillator where

-- monad-bayes
import Control.Monad.Bayes.Class hiding (posterior, prior)
import Control.Monad.Bayes.Population hiding (hoist)

-- rhine
import FRP.Rhine

-- rhine-bayes
import FRP.Rhine.Bayes

type Temperature = Double
type Pos = (Double, Double)
type Sensor = Pos


-- * Model

-- ** Prior

-- | Harmonic oscillator with white noise
prior1d ::
  (MonadDistribution m, Diff td ~ Double) =>
  -- | Starting position
  Double ->
  -- | Starting velocity
  Double ->
  BehaviourF m td Temperature Double
prior1d initialPosition initialVelocity = feedback 0 $ proc (temperature, position') -> do
  impulse <- arrM (normal 0) -< temperature
  let acceleration = (-3) * position' + impulse
  -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
  velocity <- arr (+ initialVelocity) <<< decayIntegral 10 -< acceleration
  position <- integralFrom initialPosition -< velocity
  returnA -< (position, position)

-- | 2D harmonic oscillator with noise
prior :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td Temperature Pos
prior = prior1d 10 0 &&& prior1d 0 10

-- ** Observation

-- | An integral where the integrated value dies of exponentially
decayIntegral :: (VectorSpace v (Diff td), Monad m, Floating (Diff td)) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = (timeConstant *^) <$> average timeConstant

-- | The assumed standard deviation of the sensor noise
sensorNoiseTemperature :: Double
sensorNoiseTemperature = 1

-- | A generative model of the sensor noise
noise :: MonadDistribution m => Behaviour m td Pos
noise = whiteNoise sensorNoiseTemperature &&& whiteNoise sensorNoiseTemperature

-- | A generative model of the sensor position, given the noise
generativeModel :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td Pos Sensor
generativeModel = proc latent -> do
  noiseNow <- noise -< ()
  returnA -< latent ^+^ noiseNow

{- | This remodels the distribution defined by `noise` as a PDF,
   as to be used in the inference later.
-}
sensorLikelihood :: Pos -> Sensor -> Log Double
sensorLikelihood (posX, posY) (sensorX, sensorY) = normalPdf posX sensorNoiseTemperature sensorX * normalPdf posY sensorNoiseTemperature sensorY

-- ** User behaviour

-- | The initial value for the temperature, and also the initial guess for the temperature inference
initialTemperature :: Temperature
initialTemperature = 7

-- | We infer the temperature by randomly moving around with a Brownian motion (Wiener process).
temperatureProcess :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td () Temperature
temperatureProcess = proc () -> do
  temperatureFactor <- wienerLogDomain 20 -< ()
  returnA -< runLogDomain temperatureFactor * initialTemperature

-- | Auxiliary conversion function belonging to the log-domain library, see https://github.com/ekmett/log-domain/issues/38
runLogDomain :: Log Double -> Double
runLogDomain = exp . ln


-- * Filtering

{- | Generate a random position and sensor value, given a temperature.
   Used for simulating a situation upon which we will perform inference.
-}
genModelWithoutTemperature :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td Temperature (Sensor, Pos)
genModelWithoutTemperature = proc temperature -> do
  latent <- prior -< temperature
  sensor <- generativeModel -< latent
  returnA -< (sensor, latent)

{- | Given sensor data, sample a latent position and a temperature, and weight them according to the likelihood of the observed sensor position.
   Used to infer position and temperature.
-}
posteriorTemperatureProcess :: (MonadMeasure m, Diff td ~ Double) => BehaviourF m td Sensor (Temperature, Pos)
posteriorTemperatureProcess = proc sensor -> do
  temperature <- temperatureProcess -< ()
  latent <- prior -< temperature
  arrM score -< sensorLikelihood latent sensor
  returnA -< (temperature, latent)

-- | A collection of all displayable inference results
data Result = Result
  { temperature :: Temperature
  , measured :: Sensor
  , latent :: Pos
  , particles :: [((Temperature, Pos), Log Double)]
  }
  deriving (Show)

-- | The number of particles used in the filter. Change according to available computing power.
nParticles :: Int
nParticles = 100

-- ** Single-rate inference

{- | Given an actual temperature, simulate a latent position and measured sensor position,
   and based on the sensor data infer the latent position and the temperature.
-}
filtered :: (Diff td ~ Double, MonadDistribution m) => BehaviourF m td Temperature Result
filtered = proc temperature -> do
  (measured, latent) <- genModelWithoutTemperature -< temperature
  particles <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measured
  returnA
    -<
      Result
        { temperature
        , measured
        , latent
        , particles
        }

-- ** Components for multi-rate inference

inferenceBehaviour :: (MonadDistribution m, Diff td ~ Double, MonadIO m) => BehaviourF m td (Temperature, (Sensor, Pos)) Result
inferenceBehaviour = proc (temperature, (measured, latent)) -> do
  particles <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measured
  returnA -< Result{temperature, measured, latent, particles}
