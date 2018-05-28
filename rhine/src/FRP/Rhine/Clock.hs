{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock
  ( module FRP.Rhine.Clock
  , module X
  )
where

-- base
import qualified Control.Category as Category

-- transformers
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)

-- dunai
import Data.MonadicStreamFunction as X hiding ((>>>^), (^>>>))

-- rhine
import FRP.Rhine.TimeDomain as X

-- * The 'Clock' type class

{- |
A clock creates a stream of time stamps and additional information,
possibly together with side effects in a monad 'm'
that cause the environment to wait until the specified time is reached.
-}
type RunningClock m time tag = MSF m () (time, tag)

{- |
When initialising a clock, the initial time is measured
(typically by means of a side effect),
and a running clock is returned.
-}
type RunningClockInit m time tag = m (RunningClock m time tag, time)

{- |
Since we want to leverage Haskell's type system to annotate signal networks by their clocks,
each clock must be an own type, 'cl'.
Different values of the same clock type should tick at the same speed,
and only differ in implementation details.
Often, clocks are singletons.
-}
class TimeDomain (Time cl) => Clock m cl where
  -- | The time domain, i.e. type of the time stamps the clock creates.
  type Time cl
  -- | Additional information that the clock may output at each tick,
  --   e.g. if a realtime promise was met, if an event occurred,
  --   if one of its subclocks (if any) ticked.
  type Tag cl
  -- | The method that produces to a clock value a running clock,
  --   i.e. an effectful stream of tagged time stamps together with an initialisation time.
  initClock
    :: cl -- ^ The clock value, containing e.g. settings or device parameters
    -> RunningClockInit m (Time cl) (Tag cl) -- ^ The stream of time stamps, and the initial time


-- * Auxiliary definitions and utilities

-- | An annotated, rich time stamp.
data TimeInfo cl = TimeInfo
  { -- | Time passed since the last tick
    sinceTick :: Diff (Time cl)
    -- | Time passed since the initialisation of the clock
  , sinceInit :: Diff (Time cl)
    -- | The absolute time of the current tick
  , absolute  :: Time cl
    -- | The tag annotation of the current tick
  , tag       :: Tag cl
  }

-- | A utility that changes the tag of a 'TimeInfo'.
retag
  :: (Time cl1 ~ Time cl2)
  => (Tag cl1 -> Tag cl2)
  -> TimeInfo cl1 -> TimeInfo cl2
retag f TimeInfo {..} = TimeInfo { tag = f tag, .. }


-- | Given a clock value and an initial time,
--   generate a stream of time stamps.
genTimeInfo
  :: (Monad m, Clock m cl)
  => cl -> Time cl
  -> MSF m (Time cl, Tag cl) (TimeInfo cl)
genTimeInfo _ initialTime = proc (absolute, tag) -> do
  lastTime <- iPre initialTime -< absolute
  returnA                      -< TimeInfo
    { sinceTick  = absolute `diffTime` lastTime
    , sinceInit = absolute `diffTime` initialTime
    , ..
    }


-- * Certain universal building blocks to produce new clocks from given ones

-- ** Rescalings of time domains

-- | A pure morphism of time domains is just a function.
type Rescaling cl time = Time cl -> time

-- | An effectful morphism of time domains is a Kleisli arrow.
--   It can use a side effect to rescale a point in one time domain
--   into another one.
type RescalingM m cl time = Time cl -> m time

-- | An effectful, stateful morphism of time domains is an 'MSF'
--   that uses side effects to rescale a point in one time domain
--   into another one.
type RescalingS m cl time tag = MSF m (Time cl, Tag cl) (time, tag)

-- | Like 'RescalingS', but allows for an initialisation
--   of the rescaling morphism, together with the initial time.
type RescalingSInit m cl time tag = Time cl -> m (RescalingS m cl time tag, time)

-- | Convert an effectful morphism of time domains into a stateful one with initialisation.
--   Think of its type as @RescalingM m cl time -> RescalingSInit m cl time tag@,
--   although this type is ambiguous.
rescaleMToSInit
  :: Monad m
  => (time1 -> m time2) -> time1 -> m (MSF m (time1, tag) (time2, tag), time2)
rescaleMToSInit rescaling time1 = (arrM rescaling *** Category.id, ) <$> rescaling time1

-- ** Applying rescalings to clocks

-- | Applying a morphism of time domains yields a new clock.
data RescaledClock cl time = RescaledClock
  { unscaledClock :: cl
  , rescale       :: Rescaling cl time
  }


instance (Monad m, TimeDomain time, Clock m cl)
      => Clock m (RescaledClock cl time) where
  type Time (RescaledClock cl time) = time
  type Tag  (RescaledClock cl time) = Tag cl
  initClock (RescaledClock cl f) = do
    (runningClock, initTime) <- initClock cl
    return
      ( runningClock >>> first (arr f)
      , f initTime
      )

-- | Instead of a mere function as morphism of time domains,
--   we can transform one time domain into the other with an effectful morphism.
data RescaledClockM m cl time = RescaledClockM
  { unscaledClockM :: cl
  -- ^ The clock before the rescaling
  , rescaleM       :: RescalingM m cl time
  -- ^ Computing the new time effectfully from the old time
  }

instance (Monad m, TimeDomain time, Clock m cl)
      => Clock m (RescaledClockM m cl time) where
  type Time (RescaledClockM m cl time) = time
  type Tag  (RescaledClockM m cl time) = Tag cl
  initClock RescaledClockM {..} = do
    (runningClock, initTime) <- initClock unscaledClockM
    rescaledInitTime         <- rescaleM initTime
    return
      ( runningClock >>> first (arrM rescaleM)
      , rescaledInitTime
      )

-- | A 'RescaledClock' is trivially a 'RescaledClockM'.
rescaledClockToM :: Monad m => RescaledClock cl time -> RescaledClockM m cl time
rescaledClockToM RescaledClock {..} = RescaledClockM
  { unscaledClockM = unscaledClock
  , rescaleM       = return . rescale
  }


-- | Instead of a mere function as morphism of time domains,
--   we can transform one time domain into the other with a monadic stream function.
data RescaledClockS m cl time tag = RescaledClockS
  { unscaledClockS :: cl
  -- ^ The clock before the rescaling
  , rescaleS       :: RescalingSInit m cl time tag
  -- ^ The rescaling stream function, and rescaled initial time,
  --   depending on the initial time before rescaling
  }

instance (Monad m, TimeDomain time, Clock m cl)
      => Clock m (RescaledClockS m cl time tag) where
  type Time (RescaledClockS m cl time tag) = time
  type Tag  (RescaledClockS m cl time tag) = tag
  initClock RescaledClockS {..} = do
    (runningClock, initTime) <- initClock unscaledClockS
    (rescaling, rescaledInitTime) <- rescaleS initTime
    return
      ( runningClock >>> rescaling
      , rescaledInitTime
      )

-- | A 'RescaledClockM' is trivially a 'RescaledClockS'.
rescaledClockMToS
  :: Monad m
  => RescaledClockM m cl time -> RescaledClockS m cl time (Tag cl)
rescaledClockMToS RescaledClockM {..} = RescaledClockS
  { unscaledClockS = unscaledClockM
  , rescaleS       = rescaleMToSInit rescaleM
  }

-- | A 'RescaledClock' is trivially a 'RescaledClockS'.
rescaledClockToS
  :: Monad m
  => RescaledClock cl time -> RescaledClockS m cl time (Tag cl)
rescaledClockToS = rescaledClockMToS . rescaledClockToM

-- | Applying a monad morphism yields a new clock.
data HoistClock m1 m2 cl = HoistClock
  { unhoistedClock :: cl
  , monadMorphism  :: forall a . m1 a -> m2 a
  }

instance (Monad m1, Monad m2, Clock m1 cl)
      => Clock m2 (HoistClock m1 m2 cl) where
  type Time (HoistClock m1 m2 cl) = Time cl
  type Tag  (HoistClock m1 m2 cl) = Tag  cl
  initClock HoistClock {..} = do
    (runningClock, initialTime) <- monadMorphism $ initClock unhoistedClock
    let hoistMSF = liftMSFPurer
    -- TODO Look out for API changes in dunai here
    return
      ( hoistMSF monadMorphism runningClock
      , initialTime
      )

type LiftClock m t cl = HoistClock m (t m) cl

liftClock :: (Monad m, MonadTrans t) => cl -> LiftClock m t cl
liftClock unhoistedClock = HoistClock
  { monadMorphism = lift
  , ..
  }

type IOClock m cl = HoistClock IO m cl

ioClock :: MonadIO m => cl -> IOClock m cl
ioClock unhoistedClock = HoistClock
  { monadMorphism = liftIO
  , ..
  }
