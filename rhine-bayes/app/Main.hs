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

-- mmorph
import Control.Monad.Morph

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class hiding (posterior, prior)
import Control.Monad.Bayes.Population hiding (hoist)
import Control.Monad.Bayes.Sampler.Strict

-- dunai
import Control.Monad.Trans.MSF.Except

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
temperatureProcess :: (MonadDistribution m, Diff td ~ Double) => Behaviour m td Temperature
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
  , particlesPosition :: [(Pos, Log Double)]
  , particlesTemperature :: [(Temperature, Log Double)]
  }
  deriving (Show)

-- | A 'Result' where nothing has been inferred yet
emptyResult =
  Result
    { temperature = initialTemperature
    , measured = zeroVector
    , latent = zeroVector
    , particlesPosition = []
    , particlesTemperature = []
    }

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
visualisation = proc Result {temperature, measured, latent, particlesPosition, particlesTemperature} -> do
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
                  , printf "Particles: %i" $ length particlesPosition
                  , printf "Time: %.1f" time
                  ]
              return $ translate 0 ((-150) * n) $ text message
          , color red $ rectangleUpperSolid thermometerWidth $ double2Float temperature * thermometerScale
          ]
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, green)
  drawParticles -< particlesPosition
  drawParticlesTemperature -< particlesTemperature

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

drawParticle :: BehaviourF App td (Pos, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) white)

drawParticleTemperature :: BehaviourF App td (Temperature, Log Double) ()
drawParticleTemperature = proc (temperature, probability) -> do
  arrMCl paintIO -< toThermometer $ translate 0 (double2Float temperature * thermometerScale) $ color (withAlpha (double2Float $ exp $ 0.2 * ln probability) white) $ rectangleSolid thermometerWidth 2

drawParticles :: BehaviourF App td [(Pos, Log Double)] ()
drawParticles = proc particlesPosition -> do
  case particlesPosition of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

-- FIXME abstract using a library
drawParticlesTemperature :: BehaviourF App td [(Temperature, Log Double)] ()
drawParticlesTemperature = proc particlesPosition -> do
  case particlesPosition of
    [] -> returnA -< ()
    p : ps -> do
      drawParticleTemperature -< p
      drawParticlesTemperature -< ps

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
  , ("single rate, parameter collapse", mainSingleRateCollapse)
  , ("multi rate, temperature process", mainMultiRate)
  , ("multi rate, inference buffer", mainMultiRateInferenceBuffer)
  ]

main :: IO ()
main = do
  putStrLn $ ("Choose between: " ++) $ unwords $ zipWith (\n (title, _program) -> "\n" ++ show n ++ ": " ++ title) [1 ..] mains
  choice <- read <$> getLine
  map snd mains !! (choice - 1)

-- ** Single-rate : One simulation step = one inference step = one display step

-- *** Poor attempt at temperature inference: Particle collapse

-- | Choose an exponential distribution as prior for the temperature
temperaturePrior :: (MonadDistribution m) => m Temperature
temperaturePrior = gamma 1 7

{- | On startup, sample values from the temperature prior.
  Then keep sampling from the position prior and condition by the likelihood of the measured sensor position.
-}
posteriorTemperatureCollapse :: (MonadMeasure m, Diff td ~ Double) => BehaviourF m td Sensor (Temperature, Pos)
posteriorTemperatureCollapse = proc sensor -> do
  temperature <- performOnFirstSample (arr_ <$> temperaturePrior) -< ()
  latent <- prior -< temperature
  arrM score -< sensorLikelihood latent sensor
  returnA -< (temperature, latent)

{- | Given an actual temperature, simulate a latent position and measured sensor position,
   and based on the sensor data infer the latent position and the temperature.
-}
filteredCollapse :: (Diff td ~ Double) => BehaviourF App td Temperature Result
filteredCollapse = proc temperature -> do
  (measured, latent) <- genModelWithoutTemperature -< temperature
  particlesAndTemperature <- runPopulationCl nParticles resampleSystematic posteriorTemperatureCollapse -< measured
  returnA
    -<
      Result
        { temperature
        , measured
        , latent
        , particlesPosition = first snd <$> particlesAndTemperature
        , particlesTemperature = first fst <$> particlesAndTemperature
        }

-- | Run simulation, inference, and visualization synchronously
mainClSFCollapse :: (Diff td ~ Double) => BehaviourF App td () ()
mainClSFCollapse = proc () -> do
  output <- filteredCollapse -< initialTemperature
  visualisation -< output

mainSingleRateCollapse =
  void $
    sampleIO $
      launchInGlossThread glossSettings $
        reactimateCl glossClock mainClSFCollapse

-- *** Infer temperature with a stochastic process

{- | Given an actual temperature, simulate a latent position and measured sensor position,
   and based on the sensor data infer the latent position and the temperature.
-}
filtered :: (Diff td ~ Double) => BehaviourF App td Temperature Result
filtered = proc temperature -> do
  (measured, latent) <- genModelWithoutTemperature -< temperature
  positionsAndTemperatures <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measured
  returnA
    -<
      Result
        { temperature
        , measured
        , latent
        , particlesPosition = first snd <$> positionsAndTemperatures
        , particlesTemperature = first fst <$> positionsAndTemperatures
        }

-- | Run simulation, inference, and visualization synchronously
mainClSF :: (Diff td ~ Double) => BehaviourF App td () ()
mainClSF = proc () -> do
  output <- filtered -< initialTemperature
  visualisation -< output

mainSingleRate =
  void $
    sampleIO $
      launchInGlossThread glossSettings $
        reactimateCl glossClock mainClSF

-- ** Multi-rate: Simulation, inference, display at different rates

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
      positionsAndTemperatures <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measured
      returnA
        -<
          Result
            { temperature
            , measured
            , latent
            , particlesPosition = first snd <$> positionsAndTemperatures
            , particlesTemperature = first fst <$> positionsAndTemperatures
            }

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
            >-- keepLast emptyResult -->
              visualisationRhine
{- FOURMOLU_ENABLE -}

mainMultiRate :: IO ()
mainMultiRate =
  void $
    launchInGlossThread glossSettings $
      flow mainRhineMultiRate

-- ** Multi-rate: Inference in separate buffer

mainRhineMultiRateInferenceBuffer =
  userTemperature
    @@ glossClockUTC GlossEventClockIO
    >-- keepLast initialTemperature
    --> modelRhine
    @>>^ (\(temperature, (sensor, pos)) -> (sensor, (temperature, sensor, pos)))
    >-- hoistResamplingBuffer sampleIOGloss (inferenceBuffer nParticles resampleSystematic (temperatureProcess >-> (prior &&& clId)) (\(pos, _) sensor -> sensorLikelihood pos sensor))
    *-* keepLast (initialTemperature, zeroVector, zeroVector)
    --> ( \(particles, (temperature, measured, latent)) ->
            Result
              { temperature
              , measured
              , latent
              , particlesPosition = second (const (1 / fromIntegral nParticles)) <$> particles
              , particlesTemperature = (, 1 / fromIntegral nParticles) . snd <$> particles
              }
        )
    ^>>@ visualisationRhine

mainMultiRateInferenceBuffer :: IO ()
mainMultiRateInferenceBuffer =
  void $
    launchInGlossThread glossSettings $
      flow mainRhineMultiRateInferenceBuffer

-- * Utilities

instance (MonadDistribution m) => MonadDistribution (GlossConcT m) where
  random = lift random

instance (MonadFactor m) => MonadFactor (GlossConcT m) where
  score = lift . score

instance (MonadMeasure m) => MonadMeasure (GlossConcT m)

sampleIOGloss :: App a -> GlossConcT IO a
sampleIOGloss = hoist sampleIO
