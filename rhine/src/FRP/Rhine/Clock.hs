{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock
  ( module FRP.Rhine.Clock
  , module FRP.Rhine.TimeDomain
  , module Data.MonadicStreamFunction
  )
where

-- transformers
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Class (lift, MonadTrans)

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.TimeDomain

-- * The 'Clock' type class

{- |
A clock creates a stream of time stamps,
possibly together with side effects in a monad 'm'
that cause the environment to wait until the specified time is reached.

Since we want to leverage Haskell's type system to annotate signal functions by their clocks,
each clock must be an own type, 'cl'.
Different values of the same clock type should tick at the same speed,
and only differ in implementation details.
Often, clocks are singletons.
-}
class TimeDomain (TimeDomainOf cl) => Clock m cl where
  -- | The time domain, i.e. type of the time stamps the clock creates.
  type TimeDomainOf cl
  -- | Additional information that the clock may output at each tick,
  --   e.g. if a realtime promise was met, if an event occurred,
  --   if one of its subclocks (if any) ticked.
  type Tag cl
  -- | The method that produces to a clock value a running clock,
  --   i.e. an effectful stream of tagged time stamps together with an initialisation time.
  startClock
    :: cl -- ^ The clock value, containing e.g. settings or device parameters
    -> m (MSF m () (TimeDomainOf cl, Tag cl), TimeDomainOf cl) -- ^ The stream of time stamps, and the initial time


-- * Auxiliary definitions and utilities

-- | An annotated, rich time stamp.
data TimeInfo cl = TimeInfo
  { -- | Time passed since the last tick
    sinceTick  :: Diff (TimeDomainOf cl)
    -- | Time passed since the initialisation of the clock
  , sinceStart :: Diff (TimeDomainOf cl)
    -- | The absolute time of the current tick
  , absolute   :: TimeDomainOf cl
    -- | The tag annotation of the current tick
  , tag        :: Tag cl
  }

-- | A utility that changes the tag of a 'TimeInfo'.
retag
  :: (TimeDomainOf cl1 ~ TimeDomainOf cl2)
  => (Tag cl1 -> Tag cl2)
  -> TimeInfo cl1 -> TimeInfo cl2
retag f TimeInfo {..} = TimeInfo { tag = f tag, .. }


-- | Given a clock value and an initial time,
--   generate a stream of time stamps.
genTimeInfo
  :: (Monad m, Clock m cl)
  => cl -> TimeDomainOf cl
  -> MSF m (TimeDomainOf cl, Tag cl) (TimeInfo cl)
genTimeInfo _ initialTime = proc (absolute, tag) -> do
  lastTime <- iPre initialTime -< absolute
  returnA                      -< TimeInfo
    { sinceTick  = absolute `diffTime` lastTime
    , sinceStart = absolute `diffTime` initialTime
    , ..
    }


-- * Certain universal building blocks to produce new clocks from given ones

-- | Applying a morphism of time domains yields a new clock.
data RescaledClock cl td = RescaledClock
  { unscaledClock :: cl
  , rescale       :: TimeDomainOf cl -> td
  }


instance (Monad m, TimeDomain td, Clock m cl)
      => Clock m (RescaledClock cl td) where
  type TimeDomainOf (RescaledClock cl td) = td
  type Tag          (RescaledClock cl td) = Tag cl
  startClock (RescaledClock cl f) = do
    (runningClock, initTime) <- startClock cl
    return
      ( runningClock >>> first (arr f)
      , f initTime
      )

-- | Instead of a mere function as morphism of time domains,
--   we can transform one time domain into the other with Kleisli arrow.
data RescaledClockM m cl td = RescaledClockM
  { unscaledClockM :: cl
  -- ^ The clock before the rescaling
  , rescaleM       :: TimeDomainOf cl
                   -> m td
  -- ^ Computing the new time effectfully from the old time
  }

instance (Monad m, TimeDomain td, Clock m cl)
      => Clock m (RescaledClockM m cl td) where
  type TimeDomainOf (RescaledClockM m cl td) = td
  type Tag          (RescaledClockM m cl td) = Tag cl
  startClock RescaledClockM {..} = do
    (runningClock, initTime) <- startClock unscaledClockM
    rescaledInitTime         <- rescaleM initTime
    return
      ( runningClock >>> first (arrM rescaleM)
      , rescaledInitTime
      )

-- | Instead of a mere function as morphism of time domains,
--   we can transform one time domain into the other with a monadic stream function.
data RescaledClockS m cl td tag = RescaledClockS
  { unscaledClockS :: cl
  -- ^ The clock before the rescaling
  , rescaleS       :: TimeDomainOf cl
                   -> m (MSF m (TimeDomainOf cl, Tag cl) (td, tag), td)
  -- ^ The rescaling stream function, and rescaled initial time,
  --   depending on the initial time before rescaling
  }

instance (Monad m, TimeDomain td, Clock m cl)
      => Clock m (RescaledClockS m cl td tag) where
  type TimeDomainOf (RescaledClockS m cl td tag) = td
  type Tag          (RescaledClockS m cl td tag) = tag
  startClock RescaledClockS {..} = do
    (runningClock, initTime) <- startClock unscaledClockS
    (rescaling, rescaledInitTime) <- rescaleS initTime
    return
      ( runningClock >>> rescaling
      , rescaledInitTime
      )


-- | Applying a monad morphism yields a new clock.
data HoistClock m1 m2 cl = HoistClock
  { hoistedClock  :: cl
  , monadMorphism :: forall a . m1 a -> m2 a
  }

instance (Monad m1, Monad m2, Clock m1 cl)
      => Clock m2 (HoistClock m1 m2 cl) where
  type TimeDomainOf (HoistClock m1 m2 cl) = TimeDomainOf cl
  type Tag          (HoistClock m1 m2 cl) = Tag          cl
  startClock HoistClock {..} = do
    (runningClock, initialTime) <- monadMorphism $ startClock hoistedClock
    let hoistMSF = liftMSFPurer
    -- TODO Look out for API changes in dunai here
    return
      ( hoistMSF monadMorphism runningClock
      , initialTime
      )

type LiftClock m t cl = HoistClock m (t m) cl

liftClock :: (Monad m, MonadTrans t) => cl -> LiftClock m t cl
liftClock hoistedClock = HoistClock
  { monadMorphism = lift
  , ..
  }

type IOClock m cl = HoistClock IO m cl

ioClock :: MonadIO m => cl -> IOClock m cl
ioClock hoistedClock = HoistClock
  { monadMorphism = liftIO
  , ..
  }
