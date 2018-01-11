{- | A "'Busy'" clock that ticks without waiting. -}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock.Realtime.Busy where

-- base
import Data.Time.Clock

-- rhine
import FRP.Rhine

{- |
A clock that ticks without waiting.
All time passed between ticks amounts to computation time,
side effects, time measurement and framework overhead.
-}
data Busy = Busy

instance Clock IO Busy where
  type TimeDomainOf Busy = UTCTime
  type Tag          Busy = ()

  startClock _ = do
    initialTime <- getCurrentTime
    return
      ( arrM_ getCurrentTime
        &&& arr (const ())
      , initialTime
      )
