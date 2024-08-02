{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.Clock.Util where

-- base
import Control.Arrow

-- time-domain
import Data.TimeDomain

-- automaton
import Data.Automaton (Automaton, delay)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- * Auxiliary definitions and utilities

{- | Given a clock value and an initial time,
   generate a stream of time stamps.
-}
genTimeInfo ::
  (Monad m, Clock m cl) =>
  ClockProxy cl ->
  Time cl ->
  Automaton m (Time cl, Tag cl) (TimeInfo cl)
genTimeInfo _ initialTime = proc (absolute, tag) -> do
  lastTime <- delay initialTime -< absolute
  returnA
    -<
      TimeInfo
        { sinceLast = absolute `diffTime` lastTime
        , sinceInit = absolute `diffTime` initialTime
        , ..
        }
{-# INLINE genTimeInfo #-}
