{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | A "'Busy'" clock that ticks without waiting.
module FRP.Rhine.Clock.Realtime.Busy where

-- base
import Control.Monad.IO.Class

-- time
import Data.Time.Clock

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

{- |
A clock that ticks without waiting.
All time passed between ticks amounts to computation time,
side effects, time measurement and framework overhead.
-}
data Busy = Busy

instance (MonadIO m) => Clock m Busy where
  type Time Busy = UTCTime
  type Tag Busy = ()

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM (liftIO getCurrentTime)
          &&& arr (const ())
      , initialTime
      )

instance GetClockProxy Busy
