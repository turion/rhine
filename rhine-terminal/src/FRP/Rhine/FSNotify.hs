{- | Wrapper to write @fsnotify@ applications in Rhine
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module FRP.Rhine.FSNotify
  ( WatchDirEventClock (..)
  )
  where

-- base
import Prelude hiding (putChar)
import Control.Monad (void)
import Control.Concurrent (newChan, readChan)
import Control.Concurrent.MVar ()
import Data.IORef ()

-- time
import Data.Time.Clock ( getCurrentTime )

-- transformers
import Control.Monad.IO.Class (liftIO, MonadIO)

-- rhine
import FRP.Rhine.ClSF ( constM, UTCTime, Clock(..) )
import FRP.Rhine.Clock.Proxy ( GetClockProxy )

import qualified System.FSNotify as FSNotify

-- * FSNotify clock
data WatchDirEventClock
  = WatchDirEventClock FSNotify.WatchManager FilePath FSNotify.ActionPredicate

instance MonadIO m => Clock m WatchDirEventClock
  where
    type Time WatchDirEventClock = UTCTime
    type Tag  WatchDirEventClock = FSNotify.Event

    initClock (WatchDirEventClock mgr fp ap) = do
      initialTime <- liftIO getCurrentTime
      chan <- liftIO newChan
      void $ liftIO $ FSNotify.watchTreeChan mgr fp ap chan
      return
        ( constM $ do
            time <- liftIO getCurrentTime
            event <- liftIO $ readChan chan
            return (time, event)
        , initialTime
        )

instance GetClockProxy WatchDirEventClock

instance Semigroup WatchDirEventClock where
  t <> _ = t
