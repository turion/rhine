{- |
This module provides two things:

* Clocks that tick whenever events arrive on a 'Control.Concurrent.Chan',
  and useful utilities.
* Primitives to emit events.

Note that _events work across multiple clocks_,
i.e. it is possible (and encouraged) to emit events from signals
on a different clock than the event clock.
This is in line with the Rhine philosophy that _event sources are clocks_.

Events even work well across separate threads,
and constitute the recommended way of communication between threads in Rhine.

A simple example using events and threads can be found in rhine-examples.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module FRP.Rhine.Clock.Realtime.Event
  ( module FRP.Rhine.Clock.Realtime.Event
  , module Control.Monad.IO.Class
  , newChan
  )
  where

-- base
import Control.Concurrent.Chan
import Data.Time.Clock
import Data.Semigroup

-- deepseq
import Control.DeepSeq

-- transformers
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

-- rhine
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ClSF
import FRP.Rhine.Schedule

-- * Monads allowing for event emission and handling

-- | A monad transformer in which events can be emitted onto a 'Chan'.
type EventChanT event m = ReaderT (Chan event) m

-- | Escape the 'EventChanT' layer by explicitly providing a channel
--   over which events are sent.
--   Often this is not needed, and 'runEventChanT' can be used instead.
withChan :: Chan event -> EventChanT event m a -> m a
withChan = flip runReaderT

{- | Create a channel across which events can be communicated,
and subsequently execute all event effects on this channel.

Ideally, this action is run _outside_ of 'flow',
e.g. @runEventChanT $ flow myRhine@.
This way, exactly one channel is created.

Caution: Don't use this with 'morphS',
since it would create a new channel every tick.
Instead, create one @chan :: Chan c@, e.g. with 'newChan',
and then use 'withChanS'.
-}
runEventChanT :: MonadIO m => EventChanT event m a -> m a
runEventChanT a = do
  chan <- liftIO $ newChan
  runReaderT a chan

{- | Remove ("run") an 'EventChanT' layer from the monad stack
by passing it explicitly the channel over which events are sent.

This is usually only needed if you can't use 'runEventChanT'
to create the channel.
Typically, create a @chan :: Chan c@ in your main program
before the main loop (e.g. 'flow') would be run,
then, by using this function,
pass the channel to every behaviour or 'ClSF' that wants to emit events,
and, by using 'eventClockOn', to every clock that should tick on the event.
-}
withChanS
  :: Monad m
  => Chan event
  -> ClSF (EventChanT event m) cl a b
  -> ClSF m cl a b
withChanS = flip runReaderS_

-- * Event emission

{- | Emit a single event.
This causes every 'EventClock' on the same monad to tick immediately.

Be cautious when emitting events from a signal clocked by an 'EventClock'.
Nothing prevents you from emitting more events than are handled,
causing the event buffer to grow indefinitely.
-}
emit :: MonadIO m => event -> EventChanT event m ()
emit event = do
  chan <- ask
  liftIO $ writeChan chan event

-- | Emit an event on every tick.
emitS :: MonadIO m => ClSF (EventChanT event m) cl event ()
emitS = arrMCl emit

-- | Emit an event whenever the input value is @Just event@.
emitSMaybe :: MonadIO m => ClSF (EventChanT event m) cl (Maybe event) ()
emitSMaybe = mapMaybe emitS >>> arr (const ())

-- | Like 'emit', but completely evaluates the event before emitting it.
emit' :: (NFData event, MonadIO m) => event -> EventChanT event m ()
emit' event = event `deepseq` do
  chan <- ask
  liftIO $ writeChan chan event

-- | Like 'emitS', but completely evaluates the event before emitting it.
emitS' :: (NFData event, MonadIO m) => ClSF (EventChanT event m) cl event ()
emitS' = arrMCl emit'

-- | Like 'emitSMaybe', but completely evaluates the event before emitting it.
emitSMaybe'
  :: (NFData event, MonadIO m)
  => ClSF (EventChanT event m) cl (Maybe event) ()
emitSMaybe' = mapMaybe emitS' >>> arr (const ())


-- * Event clocks and schedules

-- | A clock that ticks whenever an @event@ is emitted.
--   It is not yet bound to a specific channel,
--   since ideally, the correct channel is created automatically
--   by 'runEventChanT'.
--   If you want to create the channel manually and bind the clock to it,
--   use 'eventClockOn'.
data EventClock event = EventClock

instance Semigroup (EventClock event) where
  (<>) _ _ = EventClock

instance MonadIO m => Clock (EventChanT event m) (EventClock event) where
  type Time (EventClock event) = UTCTime
  type Tag  (EventClock event) = event
  initClock _ = do
    initialTime <- liftIO getCurrentTime
    return
      ( constM $ do
          chan  <- ask
          event <- liftIO $ readChan chan
          time  <- liftIO $ getCurrentTime
          return (time, event)
      , initialTime
      )

instance GetClockProxy (EventClock event)

-- | Create an event clock that is bound to a specific event channel.
--   This is usually only useful if you can't apply 'runEventChanT'
--   to the main loop (see 'withChanS').
eventClockOn
  :: MonadIO m
  => Chan event
  -> HoistClock (EventChanT event m) m (EventClock event)
eventClockOn chan = HoistClock
  { unhoistedClock = EventClock
  , monadMorphism  = withChan chan
  }
