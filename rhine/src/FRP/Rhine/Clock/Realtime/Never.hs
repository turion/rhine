{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A clock that never ticks.
module FRP.Rhine.Clock.Realtime.Never where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Data.Void (Void)

-- time
import Data.Time.Clock

-- automaton
import Data.Automaton (constM)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- | A clock that never ticks.
data Never = Never

instance (MonadIO m) => Clock m Never where
  type Time Never = UTCTime
  type Tag Never = Void

  initClock _ = constM (liftIO . forever . threadDelay $ 10 ^ 9)

instance GetClockProxy Never
