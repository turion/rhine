{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.Clock.Util where

-- time-domain
import Data.TimeDomain

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- * Auxiliary definitions and utilities

-- | Given a clock value and an initial time,
--   generate a stream of time stamps.
genTimeInfo
  :: (Monad m, Clock m cl)
  => ClockProxy cl -> Time cl
  -> MSF m (Time cl, Tag cl) (TimeInfo cl)
genTimeInfo _ initialTime = proc (absolute, tag) -> do
  lastTime <- iPre initialTime -< absolute
  returnA                      -< TimeInfo
    { sinceLast = absolute `diffTime` lastTime
    , sinceInit = absolute `diffTime` initialTime
    , ..
    }
