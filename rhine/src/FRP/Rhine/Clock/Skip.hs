{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Clock.Skip where

import Data.Automaton (hoistS)
import Data.Automaton.Schedule (SkipT, runSkipT)
import Data.TimeDomain (TimeDomain)
import FRP.Rhine.Clock (Clock (..))

newtype SkipClock cl = SkipClock {getSkipClock :: cl}

instance (TimeDomain (Time cl), Clock (SkipT m) cl, Monad m) => Clock m (SkipClock cl) where
  type Time (SkipClock cl) = Time cl
  type Tag (SkipClock cl) = Tag cl

  initClock SkipClock {getSkipClock} = do
    (runningClock, initialTime) <- runSkipT $ initClock getSkipClock
    pure
      ( hoistS runSkipT runningClock
      , initialTime
      )
  {-# INLINE initClock #-}
