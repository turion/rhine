{- | Wrapper to write @terminal@ applications in Rhine, using concurrency.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.Terminal
  ( TerminalEventClock (..)
  , flowTerminal
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
    ( awaitEvent, runTerminalT, Event, Interrupt, TerminalT )
import System.Terminal.Internal ( Terminal )

-- transformers
import Control.Monad.Trans.Reader

-- rhine
import FRP.Rhine.Clock.Proxy ()
import FRP.Rhine
import Control.Monad.Trans.Class (lift)

-- * Terminal clock
data TerminalEventClock = TerminalEventClock

instance (Terminal t) => Clock (TerminalT t IO) TerminalEventClock
  where
    type Time TerminalEventClock = UTCTime
    type Tag  TerminalEventClock = Either Interrupt Event

    initClock TerminalEventClock = do
      initialTime <- liftIO getCurrentTime
      return
        ( constM $ do
            event <- awaitEvent
            time <- liftIO getCurrentTime
            return (time, event)
        , initialTime
        )

instance GetClockProxy TerminalEventClock

instance Semigroup TerminalEventClock where
  t <> _ = t

-- type TerminalClSF t m = ClSF (TerminalT t m) TerminalEventClock () (Either Interrupt Event)

flowTerminal
  :: ( Terminal t
     , Clock (TerminalT t IO) cl
     , GetClockProxy cl
     , Time cl ~ Time (In  cl)
     , Time cl ~ Time (Out cl)
     )
  => t
  -> Rhine (TerminalT t IO) cl () ()
  -> IO ()
flowTerminal term clsf = flip runTerminalT term $ flow clsf

-- | A schedule in the 'TerminalT LocalTerminal' transformer,
--   supplying the same backend connection to its scheduled clocks.
terminalConcurrently
  :: forall t cl1 cl2. (
       Terminal t
     , Clock (TerminalT t IO) cl1
     , Clock (TerminalT t IO) cl2
     , Time cl1 ~ Time cl2
     )
  => Schedule (TerminalT t IO) cl1 cl2
terminalConcurrently
  = Schedule $ \cl1 cl2 -> do
      term <- TerminalT ask
      lift $ first liftTransS <$>
        initSchedule concurrently (runTerminalClock term cl1) (runTerminalClock term cl2)

type RunTerminalClock m t cl = HoistClock (TerminalT t m) m cl

runTerminalClock
  :: Terminal t
  => t
  -> cl
  -> RunTerminalClock IO t cl
runTerminalClock term unhoistedClock = HoistClock
  { monadMorphism = flip runTerminalT term
  , ..
  }
