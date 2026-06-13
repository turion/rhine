{-# LANGUAGE UndecidableInstances #-}

module FRP.Rhine.Clock.Skip where

import Data.Automaton (hoistS)
import Data.Automaton.Schedule.Trans (SkipT, runSkipT)
import Data.TimeDomain (TimeDomain)
import FRP.Rhine.Clock (Clock (..))

newtype SkipClock cl = SkipClock {getSkipClock :: cl}

instance (TimeDomain (Time cl), Clock (SkipT m) cl, Monad m) => Clock m (SkipClock cl) where
  type Time (SkipClock cl) = Time cl
  type Tag (SkipClock cl) = Tag cl

  runClock = proc  SkipClock {getSkipClock} -> do
    hoistS runSkipT runClock -< getSkipClock
  {-# INLINE runClock #-}
