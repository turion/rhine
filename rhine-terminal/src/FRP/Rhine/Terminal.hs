{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Wrapper to write @terminal@ applications in Rhine, using concurrency.
module FRP.Rhine.Terminal (
  TerminalEventClock (..),
  flowTerminal,
  RunTerminalClock,
  runTerminalClock,
) where

-- base

import Unsafe.Coerce (unsafeCoerce)
import Prelude hiding (putChar)

-- exceptions
import Control.Monad.Catch (MonadMask)

-- time
import Data.Time.Clock (getCurrentTime)

-- terminal
import System.Terminal (Event, Interrupt, MonadInput, TerminalT, awaitEvent, runTerminalT)
import System.Terminal.Internal (Terminal)

-- transformers
import Control.Monad.Trans.Reader

-- monad-schedule
import Control.Monad.Schedule.Class

-- rhine
import FRP.Rhine

-- | A clock that ticks whenever events or interrupts on the terminal arrive.
data TerminalEventClock = TerminalEventClock

instance (MonadInput m, MonadIO m) => Clock m TerminalEventClock where
  type Time TerminalEventClock = UTCTime
  type Tag TerminalEventClock = Either Interrupt Event

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

{- | A function wrapping `flow` to use at the top level
 in order to run a `Rhine (TerminalT t m) cl ()`

 Example:

 @
 mainRhine :: MonadIO m => Rhine (TerminalT LocalTerminal m) TerminalEventClock () ()
 mainRhine = tagS >-> arrMCl (liftIO . print) @@ TerminalEventClock

 main :: IO ()
 main = withTerminal $ \term -> `flowTerminal` term mainRhine
 @
-}
flowTerminal ::
  ( MonadIO m
  , MonadMask m
  , Terminal t
  , Clock (TerminalT t m) cl
  , GetClockProxy cl
  , Time cl ~ Time (In cl)
  , Time cl ~ Time (Out cl)
  ) =>
  t ->
  Rhine (TerminalT t m) cl () () ->
  m ()
flowTerminal term clsf = flip runTerminalT term $ flow clsf

{- | To escape the 'TerminalT' transformer,
  you can apply this operator to your clock type,
  where @cl@ is a clock in 'TerminalT'.
  The resulting clock is then in @m@.
-}
type RunTerminalClock m t cl = HoistClock (TerminalT t m) m cl

-- | See 'RunTerminalClock'. Apply this to your clock value to remove a 'TerminalT' layer.
runTerminalClock ::
  Terminal t =>
  t ->
  cl ->
  RunTerminalClock IO t cl
runTerminalClock term unhoistedClock =
  HoistClock
    { monadMorphism = flip runTerminalT term
    , ..
    }

-- Workaround TerminalT constructor not being exported. Should be safe in practice.
-- See PR upstream https://github.com/lpeterse/haskell-terminal/pull/18
terminalT :: ReaderT t m a -> TerminalT t m a
terminalT = unsafeCoerce

unTerminalT :: TerminalT t m a -> ReaderT t m a
unTerminalT = unsafeCoerce

instance (Monad m, MonadSchedule m) => MonadSchedule (TerminalT t m) where
  schedule = terminalT . fmap (fmap (fmap terminalT)) . schedule . fmap unTerminalT
