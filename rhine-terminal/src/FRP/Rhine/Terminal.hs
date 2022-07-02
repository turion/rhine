{- | Wrapper to write @terminal@ applications in Rhine, using concurrency.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module FRP.Rhine.Terminal
  ( TerminalEventClock (..)
  )
  where

-- base
import Prelude hiding (putChar)
import Control.Concurrent ()
import Control.Concurrent.MVar ()
import Data.IORef ()

-- time
import Data.Time.Clock ( getCurrentTime )

-- terminal
import System.Terminal
import System.Terminal.Internal ( Terminal )

-- transformers
import Control.Monad.IO.Class (liftIO, MonadIO)

-- rhine
import FRP.Rhine.ClSF ( constM, UTCTime, Clock(..) )
import FRP.Rhine.Clock.Proxy ( GetClockProxy )

-- * Terminal clock
newtype TerminalEventClock t = TerminalEventClock t

instance (Terminal t, MonadIO m) => Clock m (TerminalEventClock t)
  where
    type Time (TerminalEventClock t) = UTCTime
    type Tag  (TerminalEventClock t) = Either Interrupt Event

    initClock (TerminalEventClock term) = do
      initialTime <- liftIO getCurrentTime
      return
        ( constM $ liftIO $ flip runTerminalT term $ do
            event <- awaitEvent
            time <- liftIO getCurrentTime
            return (time, event)
        , initialTime
        )

instance Terminal t => GetClockProxy (TerminalEventClock t)

instance Terminal t => Semigroup (TerminalEventClock t) where
  t <> _ = t
