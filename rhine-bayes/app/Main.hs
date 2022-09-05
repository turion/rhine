-- transformers
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

-- rhine-bayes
import FRP.Rhine.Bayes hiding (average)
import Numeric.Log hiding (sum)
import Data.Tuple (swap)

type StdDev = Double
type Pos = Double
type Sensor = Pos

model :: MonadSample m => BehaviourF m UTCTime StdDev (Sensor, Pos)
model = proc stdDev -> do
  acceleration <- arrM $ normal 3 -< stdDev
  -- Integral over roughly the last 100 seconds, dying off exponentially
  velocity <- average 10 -< acceleration
  -- Integral over velocity with very slow reset
  position <- average 10 -< velocity
  measurementError <- constM $ normal 0 3 -< ()
  returnA -< (position + measurementError, position)

sensor :: MonadSample m => BehaviourF m UTCTime StdDev Sensor
sensor = model >>> arr fst

filtered :: MonadInfer m => BehaviourF m UTCTime (StdDev, Sensor) Pos
filtered = proc (stdDev, sensor) -> do
  (estimatedOutput, latent) <- model -< stdDev
  arrM factor -< normalPdf estimatedOutput 1 sensor -- FIXME I think this is called an importance function?
  returnA -< latent
-- filtered = bayesFilter model

-- FIXME Can't do it with Has?
-- mainClSF :: (MonadIO m, MonadInfer m, Has (ExceptT ()) m) => BehaviourF m UTCTime () ()

type MySmallMonad = Sequential SamplerIO

filteredAndTrue :: BehaviourF MySmallMonad UTCTime StdDev (Pos, Pos, Pos, Sensor)
filteredAndTrue = proc stdDev -> do
  (measuredPosition, actualPosition) <- model -< stdDev
  samples <- runPopulationCl 200 resampleMultinomial filtered -< (stdDev, measuredPosition)
  -- arrM $ liftIO . print -< samples
  returnA -< (averageOf samples, stdDevOf samples, actualPosition, measuredPosition)

-- Use Statistical?
averageOf :: VectorSpace v Double => [(v, Log Double)] -> v
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

mainClSF :: BehaviourF MySmallMonad UTCTime () ()
-- mainClSF :: BehaviourF MyMonad UTCTime () ()
mainClSF = proc () -> do
  let stdDev = 10
  output <- filteredAndTrue -< stdDev
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
--   type Time MyClock = UTCTime
--   type Tag  MyClock = Bool
--   initClock MyClock = do
--     (initClock cl


main = do
  -- TODO would like push
  thing <- sampleIO
    -- $ runExceptT @()
    -- $ evidence
    -- $ smcMultinomial 10000 10
    $ finish
    $ reactimateCl cl mainClSF
  print thing
