{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}

module FRP.Rhine.Clock.Util where

-- base
import Control.Arrow

-- time-domain
import Data.TimeDomain

-- automaton
import Data.Automaton (Automaton, delay, initial)

-- rhine
import FRP.Rhine.Clock
import Data.Maybe (fromMaybe)

-- * Auxiliary definitions and utilities

{- | Given a clock value and an initial time,
   generate a stream of time stamps.
-}
genTimeInfo :: forall cl m .
  (Monad m, Clock m cl) =>
  Automaton m (Time cl, Tag cl) (TimeInfo cl)
genTimeInfo = proc (absolute, tag) -> do
  initialTime <- initial -< absolute
  lastTime <- delay Nothing -< Just absolute
  returnA
    -<
      TimeInfo
        { sinceLast = absolute `diffTime` fromMaybe initialTime lastTime
        , sinceInit = absolute `diffTime` initialTime
        , ..
        }
{-# INLINE genTimeInfo #-}
