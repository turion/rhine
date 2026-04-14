{-# LANGUAGE UndecidableInstances #-}
module FRP.Rhine.Clock.Realtime where

-- base
import Control.Arrow (arr, returnA, (>>>), (<<<))
import Control.Monad (guard)
import Control.Monad.IO.Class

-- time
import Data.Time (diffUTCTime, getCurrentTime)

-- automaton
import Data.Automaton (sumS, mappendS, sumN, liftS, constM, cached)

-- rhine
import FRP.Rhine.Clock

-- time-domain
import Data.TimeDomain (Diff, UTCTime, TimeDomain (addTime), diffTime)
import Control.Monad.Changeset.Class (MonadChangeset(..))

{- | Rescale an 'IO' clock to the UTC time domain, overwriting its timestamps.

It's the unscaled clock's responsibility to produce the waiting effect.
-}
data OverwriteUTC cl = OverwriteUTC cl

instance (Clock m cl, Monad m, MonadTime m (Time cl)) => Clock (UTCT m) (OverwriteUTC cl) where
  type Time (OverwriteUTC cl) = UTCTime
  type Tag (OverwriteUTC cl) = Tag cl
  runClock (OverwriteUTC cl) = liftS $ constM getTime >>> runClock cl >>> arr (\(_dt, tag) -> (0, tag))


{- | Rescale a clock to the UTC time domain.

The initial time stamp is measured as system time,
and the increments (durations between ticks) are taken from the original clock.
No attempt at waiting until the specified time is made,
the timestamps of the original clock are trusted unconditionally.

The original clock is responsible for creating the waiting effect.
-}
data AddUTC cl = AddUTC cl
-- addUTC :: (Real (Diff (Time cl)), MonadIO m) => cl -> AddUTC cl
-- addUTC cl = _
  -- RescaledClockM
  --   { unscaledClockM = cl
  --   -- FIXME maybe a rescaled clock that records the initial time?
  --   , rescaleTimeM = _
  --   --  const $ do
  --   --     now <- liftIO getCurrentTime
  --   --     return (arr $ \(timePassed, tag) -> (addUTCTime (realToFrac timePassed) now, tag), now)
  --   , rescaleDiffTimeM = return . realToFrac
  --   }

instance (Clock m cl, MonadTime m (Time cl), Real (Diff (Time cl))) => Clock (UTCT m) (AddUTC cl) where
  type Time (AddUTC cl) = UTCTime
  type Tag (AddUTC cl) = Tag cl
  runClock (AddUTC cl) = liftS $ proc _ -> do
    now <- constM getTime -< ()
    (dt, tag) <- runClock cl -< now
    returnA -< (realToFrac dt, tag)

{- | Like 'UTCClock', but also output in the tag whether and by how much the target realtime was missed.

The original clock specifies with its time stamps when, relative to the initialisation time,
the UTC clock should tick.
A tag of @(tag, 'Nothing')@ means that the tick was in time.
@(tag, 'Just' dt)@ means that the tick was too late by @dt@.
-}
-- type WaitUTCClock m cl = RescaledClockS m cl UTCTime (Tag cl, Maybe (Diff (Time cl)))
data WaitUTCClock cl = WaitUTCClock cl

{- | Measure the time after each tick, and wait for the remaining time until the next tick.

If the next tick should already have occurred @dt@ seconds ago,
the tag is set to @'Just' dt@, representing a failed real time attempt.

Note that this clock internally uses 'threadDelay' which can block
for quite a lot longer than the requested time, which can cause
'waitUTC' to miss one or more ticks when using a fast original clock.
When using 'threadDelay', the difference between the real wait time
and the requested wait time will be larger when using
the @-threaded@ ghc option (around 800 microseconds) than when not using
this option (around 100 microseconds). For fast clocks it is recommended
that @-threaded@ not be used in order to miss less ticks. The clock will adjust
the wait time, up to no wait time at all, to catch up when a tick is missed.
-}
-- waitUTC :: (Fractional (Time cl), Real (Diff (Time cl)), Fractional (Diff (Time cl))) => cl -> WaitUTCClock cl
-- waitUTC unscaledClockS = _
  -- RescaledClockS
  --   { unscaledClockS
  --   , rescaleSTime = arr $ \(initTime, time) -> realToFrac $ time `diffUTCTime` initTime
  --   , rescaleSDiffTime = proc (initTime, lastTime, dTime, tag) -> do
  --       let diffTimeRescaled = realToFrac dTime -- FIXME Is this really a good function to use
  --       targetTimeDiff <- sumN -< diffTimeRescaled
  --       let targetTime = addTime initTime targetTimeDiff
  --           waitTime = targetTime `diffTime` lastTime
  --       returnA -< (waitTime, (tag, guard (waitTime < 0) >> return (realToFrac waitTime)))
  --           -- let
  --           --   diff :: Rational
  --           --   diff = toRational $ beforeSleep `diffUTCTime` initTime
  --           --   remaining = toRational initTime - diff
  --           -- threadDelay $ round $ 1000000 * remaining
  --           -- now <- getCurrentTime
  --           -- return _ -- (now, (tag, guard (remaining > 0) >> return (fromRational remaining)))
  --   }

instance (Monad m, Clock m cl, MonadTime m (Time cl), Real (Diff (Time cl))) => Clock (UTCT m) (WaitUTCClock cl) where
  type Time (WaitUTCClock cl) = UTCTime
  type Tag (WaitUTCClock cl) = (Tag cl, Maybe Double)
  runClock (WaitUTCClock cl) = proc utctime -> do
    initTime <- cached -< utctime
    (intentedDiffTime, tag) <- liftS (runClock cl <<< constM getTime) -< ()
    intentedDiffTimeSinceStart <- mappendS -< intentedDiffTime
    let intendedTime = initTime `addTime` realToFrac intentedDiffTimeSinceStart
        waitDiffTime = intendedTime `diffTime` utctime
    returnA -< (waitDiffTime, (tag, guard (waitDiffTime < 0) >> Just (-waitDiffTime)))
