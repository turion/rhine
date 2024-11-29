{-# LANGUAGE UndecidableInstances #-}
module FRP.Rhine.Clock.Unyield where
import FRP.Rhine.Clock (Clock (..))
import Data.Automaton.Schedule (YieldT, runYieldT)
import Data.Automaton (hoistS)
import Data.TimeDomain (TimeDomain)

newtype UnyieldClock cl = UnyieldClock {getUnyieldClock :: cl}

instance (TimeDomain (Time cl), Clock (YieldT m) cl, Monad m)  => Clock m (UnyieldClock cl) where
  type Time (UnyieldClock cl) = Time cl
  type Tag (UnyieldClock cl) = Tag cl

  initClock UnyieldClock {getUnyieldClock}  = do
    (runningClock, initialTime) <- runYieldT $ initClock getUnyieldClock
    return (hoistS runYieldT runningClock
      , initialTime)
