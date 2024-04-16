module FRP.Rhine.Clock.Trivial where

-- base
import Control.Arrow

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy (GetClockProxy)

-- | A clock that always returns the tick '()'.
data Trivial = Trivial

instance (Monad m) => Clock m Trivial where
  type Time Trivial = ()
  type Tag Trivial = ()
  initClock _ = return (arr $ const ((), ()), ())

instance GetClockProxy Trivial
