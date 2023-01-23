-- base
{-# LANGUAGE TupleSections #-}
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Monoid (Product(getProduct, Product))
import GHC.Float (float2Double, double2Float)
import Text.Printf (printf)

-- transformers
import Control.Monad.Trans.Class

-- time
import Data.Time (getCurrentTime, addUTCTime)

-- mmorph
import Control.Monad.Morph

-- log-domain
import Numeric.Log hiding (sum)

-- monad-bayes
import Control.Monad.Bayes.Class hiding (prior, posterior)
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
prior1d :: (MonadDistribution m, Diff td ~ Double) =>
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
decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

noise :: MonadDistribution m => Behaviour m td Pos
noise = let stdDev = 1 in whiteNoise stdDev &&& whiteNoise stdDev

generativeModel :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td Pos Sensor
generativeModel = proc latent -> do
  noiseNow <- noise -< ()
  returnA -< latent ^+^ noiseNow

-- ** User behaviour

initialTemperature :: StdDev
initialTemperature = 7

-- FIXME maybe the ln exp thing ough to move inside the library so users don't have to have a direct log-domain dependency?
temperatureProcess :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td () StdDev
temperatureProcess = wienerLogDomain 20 >>> arr (ln >>> exp >>> (* initialTemperature))

-- * Filtering

-- FIXME make parameters like n particles/spawn, softeq interactive!
-- That means it'll not be a ClSF anymore

model :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td () (Sensor, (StdDev, Pos))
model = proc () -> do
  stdDev <- temperatureProcess -< ()
  (sensor, pos) <- modelWithoutTemperature -< stdDev
  returnA -< (sensor, (stdDev, pos))

modelWithoutTemperature :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td StdDev (Sensor, Pos)
modelWithoutTemperature = proc stdDev -> do
  latent <- prior -< stdDev
  sensor <- generativeModel -< latent
  returnA -< (sensor, latent)

posterior :: (MonadMeasure m, Diff td ~ Double) => BehaviourF m td Sensor (StdDev, Pos)
posterior = arr ((), ) >>> bayesFilter model

-- FIXME Want ExceptT so we can exit with escape
type MySmallMonad = GlossConcT SamplerIO

data Result = Result
  { temperature :: StdDev
  , measured :: Sensor
  , latent :: Pos
  , particles :: [((StdDev, Pos), Log Double)]
  }
  deriving Show

nParticles :: Int
nParticles = 100

filtered :: Diff td ~ Double => BehaviourF MySmallMonad td () Result
filtered = proc () -> do
  (measuredPosition, (stdDev, actualPosition)) <- model -< ()
  samples <- runPopulationCl nParticles resampleSystematic posterior -< measuredPosition
  returnA -< Result
    { temperature = stdDev
    , measured = measuredPosition
    , latent = actualPosition
    , particles = samples
    }

-- * Visualization

-- FIXME visualize temperature on a scale, and draw temperature particles on that scale
-- TODO FPS counter so we can see how too many particles bog down performance.
-- Or rather decouple simulation and graphics, and then make simulation a busy loop (with a little sleep) and display the simulation rate.
visualisation :: BehaviourF MySmallMonad td Result ()
visualisation = proc Result { temperature, measured, latent, particles } -> do
  constMCl clearIO -< ()
  arrMCl paintIO -< translate (-200) 200 $ scale 0.2 0.2 $ color white $ text $ printf "Temperature: %.2f" temperature
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, green)
  drawParticles -< particles

drawBall :: BehaviourF MySmallMonad td (Pos, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl paintIO -< scale 20 20 $ uncurry translate (double2FloatTuple position) $ color theColor $ circleSolid $ double2Float width

drawParticle :: BehaviourF MySmallMonad td (Pos, Log Double) ()
drawParticle = proc (position, probability) -> do
  drawBall -< (position, 0.1, withAlpha (double2Float $ exp $ 0.2 * ln probability) white)

drawParticles :: BehaviourF MySmallMonad td [(Pos, Log Double)] ()
drawParticles = proc particles -> do
  case particles of
    [] -> returnA -< ()
    p : ps -> do
      drawParticle -< p
      drawParticles -< ps

type GlossClockUTC cl = RescaledClockS (GlossConcT IO) cl UTCTime (Tag cl)

glossClockUTC :: Real (Time cl) => cl -> GlossClockUTC cl
glossClockUTC cl = RescaledClockS
  { unscaledClockS = cl
  , rescaleS = const $ do
      now <- liftIO getCurrentTime
      return (arr $ \(timePassed, event) -> (addUTCTime (realToFrac timePassed) now, event), now)
  }

-- * Integration

main = do
  putStrLn "Choose between single rate (1) and multi rate (2):"
  choice <- read <$> getLine
  case choice of
    1 -> mainSingleRate
    2 -> mainMultiRate
    _ -> putStrLn "invalid choice" >> main

glossSettings :: GlossSettings
glossSettings = defaultSettings
  { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }

-- FIXME rename to temperature?
-- FIXME Make interactive
stdDev :: Double
stdDev = 7

-- ** Single-rate : One sim step = one infer step = one display step

mainClSF :: Diff td ~ Double => BehaviourF MySmallMonad td () ()
mainClSF = proc () -> do
  output <- filtered -< ()
  visualisation -< output

type GlossClock = RescaledClock GlossSimClockIO Double

glossClock :: GlossClock
glossClock = RescaledClock
  { unscaledClock = GlossSimClockIO
  , rescale = float2Double
  }

  -- TODO would like push
mainSingleRate = void
  $ sampleIO
  $ launchGlossThread glossSettings
  $ reactimateCl glossClock mainClSF

-- ** Multi-rate: Simulation, inference, display at different rates

-- TODO It's not so nice I need to morphS every component, but as long as I haven't gotten rid of schedules, I can't simplify this much
modelRhine :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT (Millisecond 100)) StdDev (StdDev, (Sensor, Pos))
-- modelRhine :: MonadDistribution m => Rhine m (Millisecond 100) StdDev (Sensor, Pos)
modelRhine = hoistClSF sampleIOGloss (clId &&& modelWithoutTemperature) @@ liftClock waitClock

userStdDev :: ClSF (GlossConcT IO) (GlossClockUTC GlossEventClockIO) () StdDev
userStdDev = tagS >>> arr (selector >>> fmap Product) >>> mappendS >>> arr (fmap getProduct >>> fromMaybe 1 >>> (* stdDev))
  where
    selector (EventKey (SpecialKey KeyUp) Down _ _) = Just 1.2
    selector (EventKey (SpecialKey KeyDown) Down _ _) = Just (1 / 1.2)
    selector _ = Nothing

-- FIXME I still compute inference in the same thread. I should push calculation itself to a background thread, and force it there completely.
-- A bit tricky with laziness...
-- TODO It's silly that I need GlossConcT here
inference :: Rhine (GlossConcT IO) (LiftClock IO GlossConcT Busy) (StdDev, (Sensor, Pos)) Result
-- FIXME abstract that bracket
inference = hoistClSF sampleIOGloss thing @@ liftClock Busy
  where
    thing :: (MonadDistribution m, Diff td ~ Double) => BehaviourF m td (StdDev, (Sensor, Pos)) Result
    thing = proc (stdDev, (measured, latent)) -> do
      particles <- runPopulationCl nParticles resampleSystematic posterior -< measured
      returnA -< Result { temperature = stdDev, measured, latent, particles }

-- TODO More interactive ideas:
-- * sensor
-- * Matthias Heinzel: Click somewhere to create gravity to move the point somewhere, so the generative model doesn't capture the whole actual model
-- * Enrico: Add some graphs showing model performance

visualisationRhine :: Rhine (GlossConcT IO) (GlossClockUTC GlossSimClockIO) Result ()
visualisationRhine = hoistClSF sampleIOGloss visualisation @@ glossClockUTC GlossSimClockIO

-- FIXME I need https://github.com/turion/rhine/pull/187 or similar in order to pass the model faster to the graphics even if inference takes longer
mainRhine = userStdDev @@ glossClockUTC GlossEventClockIO >-- keepLast stdDev -@- glossConcurrently --> modelRhine >-- keepLast (stdDev, (zeroVector, zeroVector)) -@- glossConcurrently --> inference >-- keepLast Result { temperature = stdDev, measured = zeroVector, latent = zeroVector, particles = []} -@- glossConcurrently --> visualisationRhine

mainMultiRate :: IO ()
mainMultiRate = void
  $ launchGlossThread glossSettings
  $ flow mainRhine

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
