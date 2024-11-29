{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
In the Rhine philosophy, _event sources are clocks_.
Often, we want to extract certain subevents from event sources,
e.g. single out only left mouse button clicks from all input device events.
This module provides a general purpose selection clock
that ticks only on certain subevents.
-}
module FRP.Rhine.Clock.Select where

-- base
import Control.Arrow
import Data.Maybe (maybeToList)

-- automaton
import Data.Automaton (Automaton, concatS)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy

{- | A clock that selects certain subevents of type 'a',
   from the tag of a main clock.

   If two 'SelectClock's would tick on the same type of subevents,
   but should not have the same type,
   one should @newtype@ the subevent.
-}
data SelectClock cl a = SelectClock
  { mainClock :: cl
  -- ^ The main clock
  -- | Return 'Nothing' if no tick of the subclock is required,
  --   or 'Just a' if the subclock should tick, with tag 'a'.
  , select :: Tag cl -> Maybe a
  }

instance (Semigroup a, Semigroup cl) => Semigroup (SelectClock cl a) where
  cl1 <> cl2 =
    SelectClock
      { mainClock = mainClock cl1 <> mainClock cl2
      , select = \tag -> select cl1 tag <> select cl2 tag
      }

instance (Monoid cl, Semigroup a) => Monoid (SelectClock cl a) where
  mempty =
    SelectClock
      { mainClock = mempty
      , select = const mempty
      }

instance (Monad m, Clock m cl) => Clock m (SelectClock cl a) where
  type Time (SelectClock cl a) = Time cl
  type Tag (SelectClock cl a) = a
  initClock SelectClock {..} = do
    (runningClock, initialTime) <- initClock mainClock
    let
      runningSelectClock = filterS $ proc _ -> do
        (time, tag) <- runningClock -< ()
        returnA -< (time,) <$> select tag
    return (runningSelectClock, initialTime)
  {-# INLINE initClock #-}

instance GetClockProxy (SelectClock cl a)

{- | Helper function that runs an 'Automaton' with 'Maybe' output
   until it returns a value.
-}
filterS :: (Monad m) => Automaton m () (Maybe b) -> Automaton m () b
filterS = concatS . (>>> arr maybeToList)
