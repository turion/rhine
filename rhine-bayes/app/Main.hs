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

-- base
import Control.Monad (void)
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
-- FIXME They should implement MMorph
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

-- | Internal utility because `gloss` operates on floats
double2FloatTuple :: (Double, Double) -> (Float, Float)
double2FloatTuple = double2Float *** double2Float

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

-- FIXME make parameters like n particles/spawn, softeq interactive!
-- That means it'll not be a ClSF anymore

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

-- FIXME Want ExceptT so we can exit with escape
{- | The monad in which our program will run.
   'SamplerIO' is for the probabilistic effects from @monad-bayes@,
   while 'GlossConcT' adds interactive effects from @gloss@.
-}
type App = GlossConcT SamplerIO

-- TODO FPS counter so we can see how too many particles bog down performance.
-- Or rather decouple simulation and graphics, and then make simulation a busy loop (with a little sleep) and display the simulation rate.
-- | Draw the results of the simulation and inference
visualisation :: Diff td ~ Double => BehaviourF App td Result ()
visualisation = proc Result{temperature, measured, latent, particles} -> do
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
filtered :: Diff td ~ Double => BehaviourF App td Temperature Result
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
mainClSF :: Diff td ~ Double => BehaviourF App td () ()
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
      launchGlossThread glossSettings $
        reactimateCl glossClock mainClSF

-- ** Multi-rate: Simulation, inference, display at different rates

-- | Rescale the gloss clocks so they will be compatible with real 'UTCTime' (needed for compatibility with 'Millisecond')
type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: Real (Time cl) => cl -> GlossClockUTC cl
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
    returnA -< Result{temperature, measured, latent, particles}

-- | Visualize the current 'Result' at a rate controlled by the @gloss@ backend, usually 30 FPS.
visualisationRhine :: Rhine (GlossConcT IO) (GlossClockUTC GlossSimClockIO) Result ()
visualisationRhine = hoistClSF sampleIOGloss visualisation @@ glossClockUTC GlossSimClockIO

-- | Compose all four asynchronous components to a single 'Rhine'.
mainRhineMultiRate =
  userTemperature
    @@ glossClockUTC GlossEventClockIO
      >-- keepLast initialTemperature -@- glossConcurrently -->
        modelRhine
        >-- keepLast (initialTemperature, (zeroVector, zeroVector)) -@- glossConcurrently -->
          inference
            >-- keepLast Result{temperature = initialTemperature, measured = zeroVector, latent = zeroVector, particles = []} -@- glossConcurrently -->
              visualisationRhine

mainMultiRate :: IO ()
mainMultiRate =
  void $
    launchGlossThread glossSettings $
      flow mainRhineMultiRate

-- * Utilities

instance MonadDistribution m => MonadDistribution (GlossConcT m) where
  random = lift random

sampleIOGloss :: App a -> GlossConcT IO a
sampleIOGloss = hoist sampleIO
-- base
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Monoid (Product (Product, getProduct))
import Data.Tuple (swap)
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

-- FIXME They should implement MMorph
import Control.Monad.Bayes.Population hiding (hoist)
import Control.Monad.Bayes.Sampler.Strict

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

-- * Model

-- ** Prior

-- | Harmonic oscillator with white noise
prior1d ::
  (MonadDistribution m, Diff td ~ Double) =>
  -- | Starting position
  Double ->
  -- | Starting velocity
  Double ->
  BehaviourF m td StdDev Double
prior1d initialPosition initialVelocity = feedback 0 $ proc (temperature, position') -> do
  impulse <- arrM (normal 0) -< temperature
  -- FIXME make -3 input, sample once at the beginning, or on every key stroke
  let acceleration = (-3) * position' + impulse
  -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
  velocity <- arr (+ initialVelocity) <<< decayIntegral 10 -< acceleration
  position <- integralFrom initialPosition -< velocity
  returnA -< (position, position)

-- | 2D harmonic oscillator with noise
prior :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td StdDev Pos
prior = prior1d 10 0 &&& prior1d 0 10

-- ** Observation

double2FloatTuple :: (Double, Double) -> (Float, Float)
double2FloatTuple = double2Float *** double2Float

decayIntegral :: (VectorSpace v (Diff td), Monad m, Floating (Diff td)) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = (timeConstant *^) <$> average timeConstant

sensorNoiseStdDev :: Double
sensorNoiseStdDev = 1

noise :: MonadDistribution m => Behaviour m td Pos
noise = whiteNoise sensorNoiseStdDev &&& whiteNoise sensorNoiseStdDev

generativeModel :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td Pos Sensor
generativeModel = proc latent -> do
  noiseNow <- noise -< ()
  returnA -< latent ^+^ noiseNow

-- | This remodels the distribution defined by `noise` as a PDF
sensorLikelihood :: Pos -> Sensor -> Log Double
sensorLikelihood (posX, posY) (sensorX, sensorY) = normalPdf posX sensorNoiseStdDev sensorX * normalPdf posY sensorNoiseStdDev sensorY

-- ** User behaviour

initialTemperature :: StdDev
initialTemperature = 7

-- | In a situation where we don't know the temperature, we assume a log-normal centered around 'initialTemperature'
temperaturePrior :: MonadDistribution m => m StdDev
temperaturePrior = exp <$> normal (log initialTemperature) 1

-- FIXME maybe the ln exp thing ough to move inside the library so users don't have to have a direct log-domain dependency?
temperatureProcess :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td () StdDev
temperatureProcess = proc () -> do
  temperatureChangeVelocity <- exp . (+ 3) <$> integral -< -0.1
  temperatureFactor <- wienerVaryingLogDomain -< temperatureChangeVelocity
  returnA -< runLogDomain temperatureFactor * initialTemperature

runLogDomain :: Log Double -> Double
runLogDomain = exp . ln

-- * Filtering

-- FIXME make parameters like n particles/spawn, softeq interactive!
-- That means it'll not be a ClSF anymore

genModelWithTemperature :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td () (Sensor, (StdDev, Pos))
genModelWithTemperature = proc () -> do
  temperature <- temperatureProcess -< ()
  (sensor, pos) <- genModelWithoutTemperature -< temperature
  returnA -< (sensor, (temperature, pos))

genModelWithoutTemperature :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td StdDev (Sensor, Pos)
genModelWithoutTemperature = proc temperature -> do
  latent <- prior -< temperature
  sensor <- generativeModel -< latent
  returnA -< (sensor, latent)

posteriorTemperatureProcess :: (MonadMeasure m, Diff td ~ Double) => BehaviourF m td Sensor (StdDev, Pos)
posteriorTemperatureProcess = proc sensor -> do
  temperature <- temperatureProcess -< ()
  pos <- posteriorWithoutTemperature -< (temperature, sensor)
  returnA -< (temperature, pos)

posteriorWithoutTemperature :: (MonadMeasure m, Diff td ~ Double) => BehaviourF m td (StdDev, Sensor) Pos
posteriorWithoutTemperature = proc (temperature, sensor) -> do
  latent <- prior -< temperature
  arrM score -< sensorLikelihood latent sensor
  returnA -< latent

-- FIXME Want ExceptT so we can exit with escape
type MySmallMonad = GlossConcT SamplerIO

data Result = Result
  { temperature :: StdDev
  , measured :: Sensor
  , latent :: Pos
  , particles :: [((StdDev, Pos), Log Double)]
  }
  deriving (Show)

nParticles :: Int
nParticles = 100

filtered :: Diff td ~ Double => BehaviourF MySmallMonad td () Result
filtered = proc () -> do
  (measuredPosition, (temperature, actualPosition)) <- genModelWithTemperature -< ()
  samples <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measuredPosition
  returnA
    -<
      Result
        { temperature = temperature
        , measured = measuredPosition
        , latent = actualPosition
        , particles = samples
        }

-- * Visualization

-- FIXME visualize temperature on a scale, and draw temperature particles on that scale

thermometerPos :: (Float, Float)
thermometerPos = (-300, -300)

toThermometer :: Picture -> Picture
toThermometer = uncurry translate thermometerPos

thermometerScale :: Float
thermometerScale = 20

thermometerWidth :: Float
thermometerWidth = 20

-- TODO FPS counter so we can see how too many particles bog down performance.
-- Or rather decouple simulation and graphics, and then make simulation a busy loop (with a little sleep) and display the simulation rate.
visualisation :: Diff td ~ Double => BehaviourF MySmallMonad td Result ()
visualisation = proc Result{temperature, measured, latent, particles} -> do
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

drawBall :: BehaviourF MySmallMonad td (Pos, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl paintIO -< scale 20 20 $ uncurry translate (double2FloatTuple position) $ color theColor $ circleSolid $ double2Float width

drawParticle :: BehaviourF MySmallMonad td ((StdDev, Pos), Log Double) ()
drawParticle = proc ((temperature, position), probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) white)
  arrMCl paintIO -< toThermometer $ translate 0 (double2Float temperature * thermometerScale) $ color (withAlpha (double2Float $ exp $ 0.2 * ln probability) white) $ rectangleSolid thermometerWidth 2

-- FIXME use arrow utils
drawParticles :: BehaviourF MySmallMonad td [((StdDev, Pos), Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: Real (Time cl) => cl -> GlossClockUTC cl
glossClockUTC cl =
  RescaledClockS
    { unscaledClockS = cl
    , rescaleS = const $ do
        now <- liftIO getCurrentTime
        return (arr $ \(timePassed, event) -> (addUTCTime (realToFrac timePassed) now, event), now)
    }

-- * Integration

mains :: [(String, IO ())]
mains =
  [ ("single rate", mainSingleRate)
  , ("multi rate, temperature process", mainMultiRateTemperatureProcess)
  , ("multi rate, simple parameter guess (particle collapse)", mainMultiRateSimpleParam)
  , ("multi rate, Dirichlet parameter guess (constant #particles)", mainMultiRateDirichletConstant)
  , ("multi rate, Dirichlet parameter guess (elastic #particles)", mainMultiRateDirichletElastic)
  ]

main :: IO ()
main = do
  putStrLn $ ("Choose between: " ++) $ unwords $ zipWith (\n (title, _program) -> "\n" ++ show n ++ ": " ++ title) [1 ..] mains
  choice <- read <$> getLine
  map snd mains !! (choice - 1)

glossSettings :: GlossSettings
glossSettings =
  defaultSettings
    { display = InWindow "rhine-bayes" (1024, 960) (10, 10)
    }

-- FIXME rename to temperature?
-- FIXME Make interactive
temperature :: Double
temperature = 7

-- ** Single-rate : One sim step = one infer step = one display step

mainClSF :: Diff td ~ Double => BehaviourF MySmallMonad td () ()
mainClSF = proc () -> do
  output <- filtered -< ()
  visualisation -< output

type GlossClock = RescaledClock GlossSimClockIO Double

glossClock :: GlossClock
glossClock =
  RescaledClock
    { unscaledClock = GlossSimClockIO
    , rescale = float2Double
    }

-- FIXME Why does the temperature change here? It seems I've mixed up inference & generative model
-- TODO would like push
mainSingleRate =
  void $
    sampleIO $
      launchGlossThread glossSettings $
        reactimateCl glossClock mainClSF

-- ** Multi-rate: Simulation, inference, display at different rates

-- TODO It's not so nice I need to morphS every component, but as long as I haven't gotten rid of schedules, I can't simplify this much
modelRhine :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT (Millisecond 100)) StdDev (StdDev, (Sensor, Pos))
-- modelRhine :: MonadDistribution m => Rhine m (Millisecond 100) StdDev (Sensor, Pos)
modelRhine = hoistClSF sampleIOGloss (clId &&& genModelWithoutTemperature) @@ liftClock waitClock

userStdDev :: ClSF (GlossConcT IO) (GlossClockUTC GlossEventClockIO) () StdDev
userStdDev = tagS >>> arr (selector >>> fmap Product) >>> mappendS >>> arr (fmap getProduct >>> fromMaybe 1 >>> (* temperature))
 where
  selector (EventKey (SpecialKey KeyUp) Down _ _) = Just 1.2
  selector (EventKey (SpecialKey KeyDown) Down _ _) = Just (1 / 1.2)
  selector _ = Nothing

-- FIXME I still compute inference in the same thread. I should push calculation itself to a background thread, and force it there completely.
-- A bit tricky with laziness...
-- TODO It's silly that I need GlossConcT here
inference :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT Busy) (StdDev, (Sensor, Pos)) Result
-- FIXME abstract that bracket
inference = hoistClSF sampleIOGloss inferenceBehaviour @@ liftClock Busy
 where
  inferenceBehaviour :: (MonadDistribution m, Diff td ~ Double, MonadIO m) => BehaviourF m td (StdDev, (Sensor, Pos)) Result
  inferenceBehaviour = proc (temperature, (measured, latent)) -> do
    particles <- runPopulationCl nParticles resampleSystematic posteriorTemperatureProcess -< measured
    returnA -< Result{temperature = temperature, measured, latent, particles}

-- TODO More interactive ideas:

-- * sensor

-- * Matthias Heinzel: Click somewhere to create gravity to move the point somewhere, so the generative model doesn't capture the whole actual model

-- * Enrico: Add some graphs showing model performance

visualisationRhine :: Rhine (GlossConcT IO) (GlossClockUTC GlossSimClockIO) Result ()
visualisationRhine = hoistClSF sampleIOGloss visualisation @@ glossClockUTC GlossSimClockIO

-- FIXME I need https://github.com/turion/rhine/pull/187 or similar in order to pass the model faster to the graphics even if inference takes longer
mainRhineTemperatureProcess = userStdDev @@ glossClockUTC GlossEventClockIO >-- keepLast temperature -@- glossConcurrently --> modelRhine >-- keepLast (temperature, (zeroVector, zeroVector)) -@- glossConcurrently --> inference >-- keepLast Result{temperature = temperature, measured = zeroVector, latent = zeroVector, particles = []} -@- glossConcurrently --> visualisationRhine

mainMultiRateTemperatureProcess :: IO ()
mainMultiRateTemperatureProcess =
  void $
    launchGlossThread glossSettings $
      flow mainRhineTemperatureProcess

inferenceSimpleParam :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT Busy) (StdDev, (Sensor, Pos)) Result
-- FIXME abstract that bracket
inferenceSimpleParam = hoistClSF sampleIOGloss inferenceBehaviour @@ liftClock Busy
 where
  inferenceBehaviour :: (MonadIO m, MonadDistribution m, Diff td ~ Double) => BehaviourF m td (StdDev, (Sensor, Pos)) Result
  inferenceBehaviour = proc (temperature, (measured, latent)) -> do
    particles <- map (first swap) <$> runPopulationParamSimple nParticles resampleSystematic temperaturePrior (bayesFilter genModelWithoutTemperature) -< measured
    returnA -< Result{temperature = temperature, measured, latent, particles}

mainRhineSimpleParam = userStdDev @@ glossClockUTC GlossEventClockIO >-- keepLast temperature -@- glossConcurrently --> modelRhine >-- keepLast (temperature, (zeroVector, zeroVector)) -@- glossConcurrently --> inferenceSimpleParam >-- keepLast Result{temperature = temperature, measured = zeroVector, latent = zeroVector, particles = []} -@- glossConcurrently --> visualisationRhine

mainMultiRateSimpleParam :: IO ()
mainMultiRateSimpleParam =
  void $
    launchGlossThread glossSettings $
      flow mainRhineSimpleParam

inferenceDirichletConstant :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT Busy) (StdDev, (Sensor, Pos)) Result
-- FIXME abstract that bracket
inferenceDirichletConstant = hoistClSF sampleIOGloss inferenceBehaviour @@ liftClock Busy
 where
  inferenceBehaviour :: (MonadIO m, MonadDistribution m, Diff td ~ Double) => BehaviourF m td (StdDev, (Sensor, Pos)) Result
  inferenceBehaviour = proc (temperature, (measured, latent)) -> do
    particles <- map (first swap) <$> runPopulationParamDirichletConstant nParticles 1 20 resampleSystematic temperaturePrior posteriorWithoutTemperature -< measured
    -- arrM $ liftIO . print -< particles
    returnA -< Result{temperature = temperature, measured, latent, particles}

mainRhineDirichletConstant = userStdDev @@ glossClockUTC GlossEventClockIO >-- keepLast temperature -@- glossConcurrently --> modelRhine >-- keepLast (temperature, (zeroVector, zeroVector)) -@- glossConcurrently --> inferenceDirichletConstant >-- keepLast Result{temperature = temperature, measured = zeroVector, latent = zeroVector, particles = []} -@- glossConcurrently --> visualisationRhine

mainMultiRateDirichletConstant :: IO ()
mainMultiRateDirichletConstant =
  void $
    launchGlossThread glossSettings $
      flow mainRhineDirichletConstant

inferenceDirichletElastic :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT Busy) (StdDev, (Sensor, Pos)) Result
-- FIXME abstract that bracket
inferenceDirichletElastic = hoistClSF sampleIOGloss inferenceBehaviour @@ liftClock Busy
 where
  inferenceBehaviour :: (MonadIO m, MonadDistribution m, Diff td ~ Double) => BehaviourF m td (StdDev, (Sensor, Pos)) Result
  inferenceBehaviour = proc (temperature, (measured, latent)) -> do
    particles <- map (first swap) <$> runPopulationParamDirichletElastic 10 0.2 5 resampleSystematic temperaturePrior posteriorWithoutTemperature -< measured
    returnA -< Result{temperature = temperature, measured, latent, particles}

mainRhineDirichletElastic = userStdDev @@ glossClockUTC GlossEventClockIO >-- keepLast temperature -@- glossConcurrently --> modelRhine >-- keepLast (temperature, (zeroVector, zeroVector)) -@- glossConcurrently --> inferenceDirichletElastic >-- keepLast Result{temperature = temperature, measured = zeroVector, latent = zeroVector, particles = []} -@- glossConcurrently --> visualisationRhine

mainMultiRateDirichletElastic :: IO ()
mainMultiRateDirichletElastic =
  void $
    launchGlossThread glossSettings $
      flow mainRhineDirichletElastic

-- * Utilities

-- FIXME there should be a newtype for lifting MonadDistribution along a transformer,
-- and then all instances derived via it

-- See, this is why we need effect frameworks.
-- Or why monad-bayes needs newtypes for monad transformers
instance MonadDistribution m => MonadDistribution (GlossConcT m) where
  random = lift random

-- FIXME Other PDs?

sampleIOGloss :: MySmallMonad a -> GlossConcT IO a
sampleIOGloss = hoist sampleIO
