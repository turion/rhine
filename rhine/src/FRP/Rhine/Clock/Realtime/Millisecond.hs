{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Provides a clock that ticks at every multiple of a fixed number of milliseconds.
-}
module FRP.Rhine.Clock.Realtime.Millisecond where

-- base
import Control.Arrow (arr, first, second, (>>>))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor ((<&>))
import GHC.TypeLits

-- time
import Data.Time.Clock

-- automaton
import Data.Automaton (count, hoistS)

-- time-domain
import Data.TimeDomain (Seconds (..))

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Clock.Realtime (WaitUTCClock, waitUTC)

{- | A clock ticking every 'n' milliseconds, in real time.

Since 'n' is in the type signature,
it is ensured that when composing two signals on a 'Millisecond' clock,
they will be driven at the same rate.

For example, @'Millisecond' 100@ ticks every 0.1 seconds, so 10 times per seconds.

The tag of this clock is 'Maybe Double',
where 'Nothing' represents successful realtime,
and @'Just' lag@ a lag (in seconds).
-}
newtype Millisecond (n :: Nat) = Millisecond (WaitUTCClock IO (RescaledClock (CountClock n) (Seconds Double)))

instance (KnownNat n, MonadIO m) => Clock m (Millisecond n) where
  type Time (Millisecond n) = UTCTime
  type Tag (Millisecond n) = Maybe Double
  initClock (Millisecond cl) = liftIO (initClock cl) <&> first ((>>> arr (second (fmap getSeconds . snd))) . hoistS liftIO)
  {-# INLINE initClock #-}

instance GetClockProxy (Millisecond n)

-- | Tries to achieve real time by using 'waitUTC', see its docs.
waitClock :: (KnownNat n) => Millisecond n
waitClock = Millisecond $ waitUTC $ RescaledClock CountClock ((/ 1000) . fromInteger . getSeconds)

data CountClock (n :: Nat) = CountClock

instance (Monad m, KnownNat n) => Clock m (CountClock n) where
  type Time (CountClock n) = Seconds Integer
  type Tag (CountClock n) = ()
  initClock cl = pure (count >>> arr ((* natVal cl) >>> Seconds >>> (,())), 0)
