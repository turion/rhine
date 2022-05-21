{- | Wrapper to write @terminal@ applications in Rhine, using concurrency.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module FRP.Rhine.Terminal
  ( TerminalEventClock (..)
  , terminalConcurrently
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
import System.Terminal.Internal ( LocalTerminal )

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

-- rhine
import FRP.Rhine.ClSF ( constM, UTCTime, Clock(..), HoistClock(..) )
import FRP.Rhine.Clock.Proxy ( GetClockProxy )
import FRP.Rhine.Schedule (Schedule(..))
import FRP.Rhine (concurrently, liftTransS, first)

-- * Terminal clock
newtype TerminalEventClock = TerminalEventClock LocalTerminal

instance Clock (TerminalT LocalTerminal IO) TerminalEventClock
  where
    type Time TerminalEventClock = UTCTime
    type Tag  TerminalEventClock = Either Interrupt Event

    initClock (TerminalEventClock term) = do
      initialTime <- liftIO getCurrentTime
      return
        ( constM $ flip runTerminalT term $ do
            event <- awaitEvent
            time <- liftIO getCurrentTime
            return (time, event)
        , initialTime
        )

instance GetClockProxy TerminalEventClock

instance Semigroup TerminalEventClock where
  t <> _ = t

-- | A schedule in the 'TerminalT LocalTerminal' transformer,
--   supplying the same backend connection to its scheduled clocks.
terminalConcurrently
  :: forall cl1 cl2. (
       Clock (TerminalT LocalTerminal IO) cl1
     , Clock (TerminalT LocalTerminal IO) cl2
     , Time cl1 ~ Time cl2
     )
  => LocalTerminal -> Schedule (TerminalT LocalTerminal IO) cl1 cl2
terminalConcurrently term
  = Schedule $ \cl1 cl2 -> lift $ first liftTransS <$>
    initSchedule concurrently (runTerminalClock term cl1) (runTerminalClock term cl2)

type RunTerminalClock m cl = HoistClock (TerminalT LocalTerminal m) m cl

runTerminalClock
  :: LocalTerminal
  -> cl
  -> RunTerminalClock IO cl
runTerminalClock term unhoistedClock = HoistClock
  { monadMorphism = flip runTerminalT term
  , ..
  }
