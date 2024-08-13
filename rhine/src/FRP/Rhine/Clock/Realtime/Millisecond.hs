{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

{- |
Provides a clock that ticks at every multiple of a fixed number of milliseconds.
-}
module FRP.Rhine.Clock.Realtime.Millisecond where

-- base
import Control.Arrow (arr, second, (>>>), Arrow ((&&&)))
import GHC.TypeLits

-- time
import Data.Time.Clock

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.FixedStep
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Realtime (WaitUTCClock, waitUTC)
import FRP.Rhine.Clock.Unschedule
import Data.Automaton (sumN)
import Data.TimeDomain (TimeDomain (..), TimeDifference (..))

{- | A clock ticking every 'n' milliseconds, in real time.

Since 'n' is in the type signature,
it is ensured that when composing two signals on a 'Millisecond' clock,
they will be driven at the same rate.

For example, @'Millisecond' 100@ ticks every 0.1 seconds, so 10 times per seconds.

The tag of this clock is 'Maybe Double',
where 'Nothing' represents successful realtime,
and @'Just' lag@ a lag (in seconds).
-}
newtype Millisecond (n :: Nat) = Millisecond (WaitUTCClock IO (RescaledClock (StupidFixedStep n) Double))

instance Clock IO (Millisecond n) where
  type Time (Millisecond n) = UTCTime
  type Tag (Millisecond n) = Maybe Double
  initClock (Millisecond cl) = initClock cl >>> arr (second snd)
  {-# INLINE initClock #-}

instance GetClockProxy (Millisecond n)

data StupidFixedStep (n :: Nat) where
  StupidFixedStep :: KnownNat n => StupidFixedStep n

instance TimeDifference Int where
  difference = (-)
  add = (+)

instance TimeDomain Int where
  type Diff Int = Int
  diffTime = (-)
  addTime = (+)

instance Monad m => Clock m (StupidFixedStep n) where
  type Time (StupidFixedStep n) = Int
  type Tag (StupidFixedStep n) = ()
  initClock cl@StupidFixedStep = let n = fromInteger $ natVal cl
    in arr (const n) >>> (sumN &&& arr (const ()))
  {-# INLINE initClock #-}

-- | Tries to achieve real time by using 'waitUTC', see its docs.
waitClock :: (KnownNat n) => Millisecond n
waitClock = Millisecond $ waitUTC $ RescaledClock StupidFixedStep ((/ 1000) . fromIntegral)
{-# INLINE waitClock #-}
