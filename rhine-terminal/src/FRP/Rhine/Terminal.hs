{- | Wrapper to write @terminal@ applications in Rhine, using concurrency.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
import System.Terminal.Internal ( Terminal )

-- transformers
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (liftIO)

-- rhine
import FRP.Rhine.ClSF ( constM, UTCTime, Clock(..), HoistClock(..) )
import FRP.Rhine.Clock.Proxy ( GetClockProxy )
import FRP.Rhine.Schedule (Schedule(..))
import FRP.Rhine (concurrently, liftTransS, first)

-- * Terminal clock
newtype TerminalEventClock t = TerminalEventClock t

instance Terminal t => Clock (TerminalT t IO) (TerminalEventClock t)
  where
    type Time (TerminalEventClock t) = UTCTime
    type Tag  (TerminalEventClock t) = Either Interrupt Event

    initClock (TerminalEventClock term) = do
      initialTime <- liftIO getCurrentTime
      return
        ( constM $ flip runTerminalT term $ do
            event <- awaitEvent
            time <- liftIO getCurrentTime
            return (time, event)
        , initialTime
        )

instance Terminal t => GetClockProxy (TerminalEventClock t)

instance Terminal t => Semigroup (TerminalEventClock t) where
  t <> _ = t

-- | A schedule in the 'TerminalT LocalTerminal' transformer,
--   supplying the same backend connection to its scheduled clocks.
terminalConcurrently
  :: forall t cl1 cl2. (
       Terminal t
     , Clock (TerminalT t IO) cl1
     , Clock (TerminalT t IO) cl2
     , Time cl1 ~ Time cl2
     )
  => t -> Schedule (TerminalT t IO) cl1 cl2
terminalConcurrently term
  = Schedule $ \cl1 cl2 -> lift $ first liftTransS <$>
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
