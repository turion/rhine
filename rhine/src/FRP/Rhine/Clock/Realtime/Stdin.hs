{- |
In Rhine, event sources are clocks, and so is the console.
If this clock is used,
every input line on the console triggers one tick of the 'StdinClock'.
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Clock.Realtime.Stdin where

-- base
import Data.Time.Clock
import Data.Semigroup

-- transformers
import Control.Monad.IO.Class

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import Data.Semigroup

{- |
A clock that ticks for every line entered on the console,
outputting the entered line as its 'Tag'.
-}
data StdinClock = StdinClock

instance MonadIO m => Clock m StdinClock where
  type Time StdinClock = UTCTime
  type Tag  StdinClock = String

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM $ liftIO $ do
          line <- getLine
          time <- getCurrentTime
          return (time, line)
      , initialTime
      )

instance GetClockProxy StdinClock

instance Semigroup StdinClock where
  _ <> _ = StdinClock
