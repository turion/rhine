{- | Interactive machine learning example.

In this example, you will find the following:

* A stochastic movement of a ball in heat bath,
  where the ball is kicked around in Brownian motion
* A noisy sensor observing the ball
* Particle filter inference trying to recover the actual (latent) position of the ball,
  as well as the temperature
* Visualization of the simulation and the inference result
* Two different architectures for the whole application:
  * A simple, noninteractive architecture where simulation, inference and visualization all run synchronously
  * A more scalable, modular, interactive architecture, where all these three systems run on separate clocks,
    and the user can interactively change the temperature of the heat bath
-}
module Main where

-- base
import Control.Monad (replicateM, void)
import Data.Maybe (fromMaybe)
import Data.Monoid (Product (Product, getProduct))
import GHC.Float (double2Float, float2Double)
import Text.Printf (printf)

-- transformers
import Control.Monad.Trans.Class

-- time
import Data.Time (addUTCTime, getCurrentTime)

-- mmorph
import Control.Monad.Morph

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class hiding (posterior, prior)
import Control.Monad.Bayes.Population hiding (hoist)
import Control.Monad.Bayes.Sampler.Strict

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.IO

-- rhine-bayes
import FRP.Rhine.Bayes
import FRP.Rhine.Gloss.Common

type Temperature = Double
type Pos = (Double, Double)
type Sensor = Pos

-- * Model

-- ** Prior

-- | Harmonic oscillator with white noise
prior1d ::
  (Diff td ~ Double) =>
  -- | Starting position
  Double ->
  -- | Starting velocity
  Double ->
  StochasticProcessF td Temperature Double
prior1d initialPosition initialVelocity = feedback 0 $ proc (temperature, position') -> do
  impulse <- whiteNoiseVarying -< temperature
  let acceleration = (-3) * position' + impulse
  -- Integral over roughly the last 10 seconds, dying off exponentially, as to model a small friction term
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
noise :: StochasticProcess td Pos
noise = whiteNoise sensorNoiseTemperature &&& whiteNoise sensorNoiseTemperature

-- | A generative model of the sensor position, given the noise
generativeModel :: (Diff td ~ Double) => StochasticProcessF td Pos Sensor
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

-- | We assume the user changes the temperature randomly every 3 seconds.
temperatureProcess :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td () Temperature
temperatureProcess =
  -- Draw events from a Poisson process with a rate of one event per 3 seconds
  poissonHomogeneous 3
    -- For every event, draw a number from a normal distribution
    >>> arrMCl (flip replicateM $ normal 0 0.2)
    -- Sum the numbers and log-transform then into the positive reals
    >>> arr (exp . sum)
    -- Multiply original temperature with the random temperature changes
    >>> accumulateWith (*) initialTemperature

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

-- * Visualization

-- | Internal utility because `gloss` operates on floats
double2FloatTuple :: (Double, Double) -> (Float, Float)
double2FloatTuple = double2Float *** double2Float

{- | The monad in which our program will run.
   'SamplerIO' is for the probabilistic effects from @monad-bayes@,
   while 'GlossConcT' adds interactive effects from @gloss@.
-}
type App = GlossConcT SamplerIO

-- | Draw the results of the simulation and inference
visualisation :: (Diff td ~ Double) => BehaviourF App td Result ()
visualisation = proc Result {temperature, measured, latent, particles} -> do
  constMCl clearIO -< ()
  time <- sinceInitS -< ()
  arrMCl paintIO
    -<
      toThermometer $
        pictures
          [ translate 0 (-40) $ scale 0.2 0.2 $ color white $ pictures $ do
              (n, message) <-
                zip
                  [0 ..]
                  [ printf "Temperature: %.2f" temperature
                  , printf "Particles: %i" $ length particles
                  , printf "Time: %.1f" time
                  ]
              return $ translate 0 ((-150) * n) $ text message
          , color red $ rectangleUpperSolid thermometerWidth $ double2Float temperature * thermometerScale
          ]
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, green)
  drawParticles -< particles

-- ** Parameters for the temperature display

thermometerPos :: (Float, Float)
thermometerPos = (-300, -300)

toThermometer :: Picture -> Picture
toThermometer = uncurry translate thermometerPos

thermometerScale :: Float
thermometerScale = 20

thermometerWidth :: Float
thermometerWidth = 20

-- ** Helpers for drawing elements of the visualization

drawBall :: BehaviourF App td (Pos, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl paintIO -< scale 20 20 $ uncurry translate (double2FloatTuple position) $ color theColor $ circleSolid $ double2Float width

drawParticle :: BehaviourF App td ((Temperature, Pos), Log Double) ()
drawParticle = proc ((temperature, position), probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) white)
  arrMCl paintIO -< toThermometer $ translate 0 (double2Float temperature * thermometerScale) $ color (withAlpha (double2Float $ exp $ 0.2 * ln probability) white) $ rectangleSolid thermometerWidth 2

drawParticles :: BehaviourF App td [((Temperature, Pos), Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

glossSettings :: GlossSettings
glossSettings =
  defaultSettings
    { display = InWindow "rhine-bayes" (1024, 960) (10, 10)
    }

-- * Integration

-- | There are different architectural choices for the whole application, these can be tested against each other
mains :: [(String, IO ())]
mains =
  [ ("single rate", mainSingleRate)
  , ("multi rate, temperature process", mainMultiRate)
  ]

main :: IO ()
main = do
  putStrLn $ ("Choose between: " ++) $ unwords $ zipWith (\n (title, _program) -> "\n" ++ show n ++ ": " ++ title) [1 ..] mains
  choice <- read <$> getLine
  map snd mains !! (choice - 1)

-- ** Single-rate : One simulation step = one inference step = one display step

{- | Given an actual temperature, simulate a latent position and measured sensor position,
   and based on the sensor data infer the latent position and the temperature.
-}
filtered :: (Diff td ~ Double) => BehaviourF App td Temperature Result
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

-- | Run simulation, inference, and visualization synchronously
mainClSF :: (Diff td ~ Double) => BehaviourF App td () ()
mainClSF = proc () -> do
  output <- filtered -< initialTemperature
  visualisation -< output

-- | Rescale to the 'Double' time domain
type GlossClock = RescaledClock GlossSimClockIO Double

glossClock :: GlossClock
glossClock =
  RescaledClock
    { unscaledClock = GlossSimClockIO
    , rescale = float2Double
    }

mainSingleRate =
  void $
    sampleIO $
      launchInGlossThread glossSettings $
        reactimateCl glossClock mainClSF

-- ** Multi-rate: Simulation, inference, display at different rates

-- | Rescale the gloss clocks so they will be compatible with real 'UTCTime' (needed for compatibility with 'Millisecond')
type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: (Real (Time cl)) => cl -> GlossClockUTC cl
glossClockUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arr $ \(timePassed, event) -> (addUTCTime (realToFrac timePassed) now, event), now)
    }

{- | The part of the program which simulates latent position and sensor,
   running 100 times a second.
-}
modelRhine :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT (Millisecond 100)) Temperature (Temperature, (Sensor, Pos))
modelRhine = hoistClSF sampleIOGloss (clId &&& genModelWithoutTemperature) @@ liftClock waitClock

-- | The user can change the temperature by pressing the up and down arrow keys.
userTemperature :: ClSF (GlossConcT IO) (GlossClockUTC GlossEventClockIO) () Temperature
userTemperature = tagS >>> arr (selector >>> fmap Product) >>> mappendS >>> arr (fmap getProduct >>> fromMaybe 1 >>> (* initialTemperature))
  where
    selector (EventKey (SpecialKey KeyUp) Down _ _) = Just 1.2
    selector (EventKey (SpecialKey KeyDown) Down _ _) = Just (1 / 1.2)
    selector _ = Nothing

{- | This part performs the inference (and passes along temperature, sensor and position simulations).
   It runs as fast as possible, so this will potentially drain the CPU.
-}
inference :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT Busy) (Temperature, (Sensor, Pos)) Result
inference = hoistClSF sampleIOGloss inferenceBehaviour @@ liftClock Busy
  where
    inferenceBehaviour :: (MonadDistribution m, Diff td ~ Double, MonadIO m) => BehaviourF m td (Temperature, (Sensor, Pos)) Result
    inferenceBehaviour = proc (temperature, (measured, latent)) -> do
      particles <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measured
      returnA -< Result {temperature, measured, latent, particles}

-- | Visualize the current 'Result' at a rate controlled by the @gloss@ backend, usually 30 FPS.
visualisationRhine :: Rhine (GlossConcT IO) (GlossClockUTC GlossSimClockIO) Result ()
visualisationRhine = hoistClSF sampleIOGloss visualisation @@ glossClockUTC GlossSimClockIO

{- FOURMOLU_DISABLE -}
-- | Compose all four asynchronous components to a single 'Rhine'.
mainRhineMultiRate =
  userTemperature
    @@ glossClockUTC GlossEventClockIO
      >-- keepLast initialTemperature -->
        modelRhine
        >-- keepLast (initialTemperature, (zeroVector, zeroVector)) -->
          inference
            >-- keepLast Result {temperature = initialTemperature, measured = zeroVector, latent = zeroVector, particles = []} -->
              visualisationRhine
{- FOURMOLU_ENABLE -}

mainMultiRate :: IO ()
mainMultiRate =
  void $
    launchInGlossThread glossSettings $
      flow mainRhineMultiRate

-- * Utilities

instance (MonadDistribution m) => MonadDistribution (GlossConcT m) where
  random = lift random

instance (MonadFactor m) => MonadFactor (GlossConcT m) where
  score = lift . score

instance (MonadMeasure m) => MonadMeasure (GlossConcT m)

sampleIOGloss :: App a -> GlossConcT IO a
sampleIOGloss = hoist sampleIO
