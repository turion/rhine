-- transformers
{-# LANGUAGE NamedFieldPuns #-}
import Control.Monad.Trans.Class

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
type Pos = Double
type Sensor = Pos

model :: (MonadSample m, Diff td ~ Float) => BehaviourF m td StdDev (Sensor, Pos)
model = proc stdDev -> do
  acceleration <- arrM $ normal 5 -< stdDev
  -- Integral over roughly the last 100 seconds, dying off exponentially
  velocity <- decayIntegral 2 -< double2Float acceleration
  -- Integral over velocity with very slow reset
  position <- decayIntegral 2 -< velocity
  measurementError <- constM $ normal 0 3 -< ()
  returnA -< (float2Double position + measurementError, float2Double position)

decayIntegral :: (VectorSpace v (Diff td), Monad m) => Diff td -> BehaviourF m td v v
decayIntegral timeConstant = average timeConstant >>> arr (timeConstant *^)

sensor :: (MonadSample m, Diff td ~ Float) => BehaviourF m td StdDev Sensor
sensor = model >>> arr fst

filtered :: (MonadInfer m, Diff td ~ Float) => BehaviourF m td (StdDev, Sensor) Pos
filtered = proc (stdDev, sensor) -> do
  (estimatedOutput, latent) <- model -< stdDev
  arrM factor -< normalPdf estimatedOutput 1 sensor -- FIXME I think this is called an importance function?
  returnA -< latent
-- filtered = bayesFilter model

-- FIXME Can't do it with Has?
-- mainClSF :: (MonadIO m, MonadInfer m, Has (ExceptT ()) m) => BehaviourF m td () ()

type MySmallMonad = Sequential (GlossConcT SamplerIO)

data Result = Result
  { estimate :: Pos
  , stdDev :: StdDev
  , measured :: Sensor
  , latent :: Pos
  }
  deriving Show

filteredAndTrue :: Diff td ~ Float => BehaviourF MySmallMonad td StdDev Result
filteredAndTrue = proc stdDev -> do
  (measuredPosition, actualPosition) <- model -< stdDev
  samples <- runPopulationCl 200 resampleMultinomial filtered -< (stdDev, measuredPosition)
  -- arrM $ liftIO . print -< samples
  returnA -< Result
    { estimate = averageOf samples
    , stdDev = stdDevOf samples
    , measured = measuredPosition
    , latent = actualPosition
    }

-- Use Statistical?
averageOf :: VectorSpace v n => [(v, Log n)] -> v
averageOf things =
  let
    properThings = first (exp . ln) . swap <$> things
    fullWeight = sum $ fst <$> properThings
    sumOfThings = foldr (^+^) zeroVector $ fmap (uncurry (*^)) properThings
  in sumOfThings ^/ fullWeight

stdDevOf :: [(Double, Log Double)] -> Double
stdDevOf things =
  let
    average = averageOf things
    squares = first (\x -> (x - average) ^ 2) <$> things
  in sqrt $ averageOf squares

visualisation :: BehaviourF MySmallMonad td Result ()
visualisation = proc Result { estimate, stdDev, measured, latent } -> do
  constMCl $ lift clearIO -< ()
  drawBall -< (estimate, stdDev, blue)
  drawBall -< (measured, 0.3, red)
  drawBall -< (latent, 0.3, withAlpha 0.5 green)

-- FIXME opacity of estimate based on total probability mass

drawBall :: BehaviourF MySmallMonad td (Double, Double, Color) ()
drawBall = proc (position, width, theColor) -> do
  arrMCl $ lift . paintIO -< scale 20 20 $ translate (double2Float position) 0 $ color theColor $ circleSolid $ double2Float width

mainClSF :: Diff td ~ Float => BehaviourF MySmallMonad td () ()
-- mainClSF :: BehaviourF MyMonad td () ()
mainClSF = proc () -> do
  let stdDev = 20
  output <- filteredAndTrue -< stdDev
  visualisation -< output
  arrM $ liftIO . print -< output
  n <- count -< ()
  arrM $ liftIO . print -< n
  -- liftHS $ throwOn () -< n > 100
  -- liftClSF $ liftClSF $ throwOn () -< n > 100

-- FIXME should be in monad-bayes
instance MonadSample m => MonadSample (ExceptT e m) where
  random = lift random

-- liftHS :: Has t m => (forall n . ClSF (t n) cl a b) -> ClSF m cl a b
-- liftHS clsf = hoistClSF liftH clsf

type MyMonad = Sequential (Population SamplerIO)
-- type MyMonad = Sequential (Population (ExceptT () SamplerIO))

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

glossClock :: LiftClock (GlossConcT SamplerIO) Sequential GlossSimClockIO
glossClock = liftClock GlossSimClockIO

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
