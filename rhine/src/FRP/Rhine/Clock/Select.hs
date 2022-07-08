{- |
In the Rhine philosophy, _event sources are clocks_.
Often, we want to extract certain subevents from event sources,
e.g. single out only left mouse button clicks from all input device events.
This module provides a general purpose selection clock
that ticks only on certain subevents.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE LiberalTypeSynonyms #-}
module FRP.Rhine.Clock.Select where

-- base
import Data.Semigroup

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

-- transformers
import Control.Monad.Trans.Class

-- monad-schedule
import Control.Monad.Schedule.Class (MonadSchedule)

-- base
import Data.Maybe (maybeToList)

-- rhine
import Control.Monad.Event

{- |
A clock that selects certain events of type 'a',
from the tag of a main clock @cl@.

Note: If two 'SelectClock's would tick on the same type of subevents,
but should not have the same type,
one should @newtype@ the subevent.
-}
data SelectClock cl a = SelectClock
  {
  -- | Return 'Nothing' if no tick of the clock is required,
  --   or 'Just a' if the subclock should tick, with tag 'a'.
  select :: Tag cl -> Maybe a
  }

instance Semigroup a => Semigroup (SelectClock cl a) where
  cl1 <> cl2 = SelectClock
    { select = \tag -> select cl1 tag <> select cl2 tag
    }

instance Semigroup a => Monoid (SelectClock cl a) where
  mempty = SelectClock
    { select = const mempty
    }

-- FIXME This doesn't work
-- instance (Monad m, Clock m cl) => Clock (EventT (Tag cl) m) (SelectClock cl a) where
instance (Clock m cl, Monad m, MonadSchedule m, ev ~ (Time cl, Maybe (Tag cl))) => Clock (EventT ev m) (SelectClock cl a) where
  type Time (SelectClock cl a) = Time cl
  type Tag  (SelectClock cl a) = a
  initClock SelectClock {..} = do
    (initialTime, _initialEvent) <- listenUntil Just
    return (constM $ listenUntil selector, initialTime)
      where
        selector :: (Time cl, Maybe (Tag cl)) -> Maybe (Time cl, a)
        selector (time, evMaybe) = do
          ev <- evMaybe
          a <- select ev
          return (time, a)

instance GetClockProxy (SelectClock cl a)

newtype EventSource cl = EventSource cl

-- I don't completely understand why this instance definition isn't fine, and I have to do a strange workaround.
-- https://gitlab.haskell.org/ghc/ghc/-/issues/3485
-- https://gitlab.haskell.org/ghc/ghc/-/issues/14046
-- instance (Monad m, MonadSchedule m, Clock m cl) => Clock (EventT (Time cl, Maybe (Tag cl)) m) (EventSource cl) where
instance (Monad m, MonadSchedule m, Clock m cl, ev ~ (Time cl, Maybe (Tag cl))) => Clock (EventT ev m) (EventSource cl) where
  type Time (EventSource cl) = Time cl
  type Tag  (EventSource cl) = Tag  cl
  initClock (EventSource cl) = do
    (runningClock, initialTime) <- lift $ initClock cl
    emit (initialTime, Nothing)
    return (liftTransS runningClock >>> arrM emitAndReturn, initialTime)
      where
        emitAndReturn :: (Time cl, Tag cl) -> EventT (Time cl, Maybe (Tag cl)) m (Time cl, Tag cl)
        emitAndReturn (time, tag) = emit (time, Just tag) >> return (time, tag)
