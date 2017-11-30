{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock.Count where


-- rhine
import FRP.Rhine

-- | A singleton clock that counts the ticks.
data Count = Count -- Sesame street anyone?

instance Monad m => Clock m Count where
  type TimeDomainOf Count = Integer
  type Tag          Count = ()
  startClock _ = return (count &&& arr (const ()), 0)
