{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A clock that never ticks.
module FRP.Rhine.Clock.Realtime.Never where

-- base
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Void (Void)

-- time
import Data.Time.Clock

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- transformers
import Control.Monad.IO.Class

-- | A clock that never ticks.
data Never = Never

instance (MonadIO m) => Clock m Never where
  type Time Never = UTCTime
  type Tag Never = Void

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM (liftIO . forever . threadDelay $ 10 ^ 9)
      , initialTime
      )

instance GetClockProxy Never
