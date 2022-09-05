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
import Numeric.Log
import Data.Tuple (swap)

type StdDev = Double
type Pos = Double
type Sensor = Pos

model :: MonadSample m => BehaviourF m UTCTime StdDev (Sensor, Pos)
model = proc stdDev -> do
  acceleration <- arrM $ normal 0 -< stdDev
  -- Integral over roughly the last 10 seconds, dying off exponentially
  velocity <- average 10 -< acceleration
  -- Integral over velocity with very slow reset
  position <- average 100 -< velocity
  measurementError <- constM $ normal 0 2 -< ()
  returnA -< (position + measurementError, position)

sensor :: MonadInfer m => BehaviourF m UTCTime StdDev Sensor
sensor = model >>> arr fst

filtered :: MonadInfer m => BehaviourF m UTCTime (StdDev, Sensor) Pos
filtered = bayesFilter model

-- FIXME Can't do it with Has?
-- mainClSF :: (MonadIO m, MonadInfer m, Has (ExceptT ()) m) => BehaviourF m UTCTime () ()

type MySmallMonad = Sequential SamplerIO

averaged :: BehaviourF MySmallMonad UTCTime StdDev (Sensor, Pos)
averaged = proc stdDev -> do
  measuredPosition <- sensor -< stdDev
  samples <- runPopulationCl filtered -< (stdDev, measuredPosition)
  returnA -< (averageOf samples, measuredPosition)

-- Use Statistical?
averageOf :: VectorSpace v Double => [(v, Log Double)] -> v
averageOf = foldr (^+^) zeroVector . fmap (uncurry (*^) . first (exp . ln) . swap)

mainClSF :: BehaviourF MySmallMonad UTCTime () ()
-- mainClSF :: BehaviourF MyMonad UTCTime () ()
mainClSF = proc () -> do
  let stdDev = 10
  -- FIXME This doesn't make sense, I have to initialise the particles at some point!
  (estimatedPosition, measurement) <- averaged -< stdDev
  -- FIXME y u no print
  arrM $ liftIO . print -< (measurement, estimatedPosition)
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
    $ smcMultinomial 10000 10
    $ reactimateCl cl mainClSF
  print thing
