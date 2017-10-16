{-# LANGUAGE Arrows         #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module FRP.Rhine.Clock.Realtime.Millisecond where

-- base
import Data.Time.Clock
import Control.Concurrent (threadDelay)
import GHC.TypeLits       (Nat, KnownNat)


-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Step

{- |
A clock ticking every 'n' milliseconds,
in real time.
Since 'n' is in the type signature,
it is ensured that when composing two signals on a 'Millisecond' clock,
they will be driven at the same rate.

The tag of this clock is 'Bool',
where 'True' represents successful realtime,
and 'False' a lag.
-}
type Millisecond (n :: Nat) = RescaledClockS IO (Step n) UTCTime Bool
-- TODO Consider changing the tag to Maybe Double

-- | This clock simply sleeps 'n' milliseconds after each tick.
--   The current time is measured, but no adjustment is made.
--   Consequently, the tag is constantly 'False',
--   since the clock will accumulate the computation time as lag.
sleepClock :: KnownNat n => Millisecond n
sleepClock = sleepClock_ Step
  where
    sleepClock_ :: Step n -> Millisecond n
    sleepClock_ cl = RescaledClockS cl $ const $ do
      now <- getCurrentTime
      return
        ( arrM_ (threadDelay (fromInteger $ stepsize cl * 1000) >> getCurrentTime)
          *** arr (const False)
        , now
        )


-- TODO Test whether realtime detection really works here,
--  e.g. with a getLine signal
-- | A more sophisticated implementation that measures the time after each tick,
--   and waits for the remaining time until the next tick.
--   If the next tick should already have occurred,
--   the tag is set to 'False', representing a failed real time attempt.
waitClock :: KnownNat n => Millisecond n
waitClock = RescaledClockS Step $ \_ -> do
  initTime <- getCurrentTime
  let
    runningClock = proc (n, ()) -> do
      beforeSleep <- arrM_ getCurrentTime -< ()
      let
        diff :: Double
        diff      = realToFrac $ beforeSleep `diffUTCTime` initTime
        remaining = fromInteger $ n * 1000 - round (diff * 1000000)
      _           <- arrM  threadDelay    -< remaining
      now         <- arrM_ getCurrentTime -< () -- TODO Test whether this is a performance penalty
      returnA                             -< (now, diff > 0)
  return (runningClock, initTime)
