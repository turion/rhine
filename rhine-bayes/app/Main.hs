-- transformers
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad.Trans.Class

-- base
import Control.Monad.Fix

-- monad-bayes
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Inference.SMC
-- FIXME They should implement MMorph
import Control.Monad.Bayes.Population hiding (hoist)
import Control.Monad.Bayes.Sampler
import Control.Monad.Bayes.Sequential

-- has-transformers
import Control.Monad.Trans.Has

-- rhine
import FRP.Rhine

-- rhine-gloss
import FRP.Rhine.Gloss.IO

-- rhine-bayes
import FRP.Rhine.Bayes hiding (average)
import Numeric.Log hiding (sum)
import Data.Tuple (swap)
import FRP.Rhine.Gloss.Common
import GHC.Float (float2Double, double2Float)

type StdDev = Double
type Pos = (Double, Double)
type Sensor = Pos


model :: (MonadSample m, Diff td ~ Double) => BehaviourF m td StdDev (Sensor, Pos)
model = feedback zeroVector $ proc (stdDev, position') -> do
  impulse <- arrM (normal 0) &&& arrM (normal 0) -< stdDev
  -- FIXME make -3 input, sample once at the beginning, or on every key stroke
  let acceleration = (-3) *^ position' ^+^ impulse
  -- Integral over roughly the last 100 seconds, dying off exponentially, as to model a small friction term
  velocity <- arr (^+^ (0, 10)) <<< decayIntegral 100 -< acceleration
  position <- integralFrom (10, 0) -< velocity
  measurementError <- constM (normal 0 5) &&& constM (normal 0 5) -< ()
  returnA -< ((position ^+^ measurementError, position), position)

double2FloatTuple :: (Double, Double) -> (Float, Float)
double2FloatTuple = double2Float *** double2Float

decayIntegral :: (VectorSpace v (Diff td), Monad m) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)


-- FIXME make parameters like n particles/spawn, softeq interactive!
-- That means it'll not be a ClSF anymore

filtered :: (MonadInfer m, Diff td ~ Double) => BehaviourF m td (StdDev, Sensor) Pos
filtered = bayesFilter model

-- FIXME Can't do it with Has?
-- mainClSF :: (MonadIO m, MonadInfer m, Has (ExceptT ()) m) => BehaviourF m td () ()

-- FIXME Want ExceptT so we can exit with escape
type MySmallMonad = Sequential (GlossConcT SamplerIO)
-- FIXME Don't need Sequential anymore?

data Result = Result
  { estimate :: Pos
  , stdDev :: StdDev
  , measured :: Sensor
  , latent :: Pos
  , particles :: [(Pos, Log Double)]
  }
  deriving Show

filteredAndTrue :: Diff td ~ Double => BehaviourF MySmallMonad td StdDev Result
filteredAndTrue = proc stdDev -> do
  (measuredPosition, actualPosition) <- model -< stdDev
  samples <- runPopulationCl 100 resampleMultinomial filtered -< (stdDev, measuredPosition)
  -- arrM $ liftIO . print -< samples
  returnA -< Result
    { estimate = averageOf samples
    , stdDev = stdDevOf samples
    , measured = measuredPosition
    , latent = actualPosition
    , particles = samples
    }

-- Use Statistical?
averageOf :: VectorSpace v n => [(v, Log n)] -> v
averageOf things =
  let
    properThings = first (exp . ln) . swap <$> things
    fullWeight = sum $ fst <$> properThings
    sumOfThings = foldr (^+^) zeroVector $ fmap (uncurry (*^)) properThings
  in sumOfThings ^/ fullWeight

stdDevOf :: [(Pos, Log Double)] -> Double
stdDevOf things =
  let
    average = averageOf things
    -- FIXME norm ^2 is wasteful
    squares = first (\x -> norm (x ^-^ average) ^ 2) <$> things
  in sqrt $ averageOf squares

-- TODO FPS counter so we can see how too many particles bog down performance.
-- Or rather decouple simulation and graphics, and then make simulation a busy loop (with a little sleep) and display the simulation rate.
visualisation :: BehaviourF MySmallMonad td Result ()
visualisation = proc Result { estimate, stdDev, measured, latent, particles } -> do
  constMCl $ lift clearIO -< ()
  -- drawBall -< (estimate, stdDev, blue)
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, green)
  drawParticles -< particles

-- FIXME opacity of estimate based on total probability mass

drawBall :: BehaviourF MySmallMonad td (Pos, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl $ lift . paintIO -< scale 20 20 $ uncurry translate (double2FloatTuple position) $ color theColor $ circleSolid $ double2Float width

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
-- mainClSF :: BehaviourF MyMonad td () ()
mainClSF = proc () -> do
  let stdDev = 5
  output <- filteredAndTrue -< stdDev
  visualisation -< output
  -- arrM $ liftIO . print -< output
  n <- count -< ()
  arrM $ liftIO . print -< n
  -- liftHS $ throwOn () -< n > 100
  -- liftClSF $ liftClSF $ throwOn () -< n > 100

-- FIXME should be in monad-bayes
-- In fact there should be a newtype for lifting MonadSample along a transformer,
-- and then all instances derived via it
instance MonadSample m => MonadSample (ExceptT e m) where
  random = lift random

-- liftHS :: Has t m => (forall n . ClSF (t n) cl a b) -> ClSF m cl a b
-- liftHS clsf = hoistClSF liftH clsf


cl :: IOClock MySmallMonad (Millisecond 100)
-- cl :: IOClock MyMonad (Millisecond 100)
cl = ioClock waitClock
-- cl :: LiftClock (Population SamplerIO) Sequential (LiftClock SamplerIO Population (Millisecond 1000))
-- cl = liftClock $ liftClock (waitClock @1000)

-- data MyClock = MyClock

-- instance MonadIO m => Clock m MyClock where
--   type Time MyClock = td
--   type Tag  MyClock = Bool
--   initClock MyClock = do
--     (initClock cl


-- See, this is why we need effect frameworks.
-- Or why monad-bayes needs newtypes for monad transformers
instance MonadSample m => MonadSample (GlossConcT m) where
  random = lift random
  -- FIXME Other PDs?

-- FIXME Make a td transformation

glossClock :: RescaledClock (LiftClock (GlossConcT SamplerIO) Sequential GlossSimClockIO) Double
glossClock = RescaledClock
  { unscaledClock = liftClock GlossSimClockIO
  , rescale = float2Double
  }

main = do
  -- TODO would like push
  thing <- sampleIO
    -- $ runExceptT @()
    -- $ evidence
    -- $ smcMultinomial 10000 10
    $ launchGlossThread defaultSettings
      { display = InWindow "rhine-bayes" (1024, 960) (10, 10) }
    $ finish
    $ reactimateCl glossClock mainClSF
  print thing
