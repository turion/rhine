{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- |
In Rhine, event sources are clocks, and so is the console.
If this clock is used,
every input line on the console triggers one tick of the 'StdinClock'.
-}
module FRP.Rhine.Clock.Realtime.Stdin where

-- time
import Data.Time.Clock

-- transformers
import Control.Monad.IO.Class

-- text
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

{- |
A clock that ticks for every line entered on the console,
outputting the entered line as its 'Tag'.
-}
data StdinClock = StdinClock

instance (MonadIO m) => Clock m StdinClock where
  type Time StdinClock = UTCTime
  type Tag StdinClock = Text.Text

  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM $ liftIO $ do
          line <- Text.getLine
          time <- getCurrentTime
          return (time, line)
      , initialTime
      )

instance GetClockProxy StdinClock

instance Semigroup StdinClock where
  _ <> _ = StdinClock
