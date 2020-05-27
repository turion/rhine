{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
In the Rhine philosophy, _event sources are clocks_.
Often, we want to extract certain subevents from event sources,
e.g. single out only left mouse button clicks from all input device events.
This module provides a general purpose selection clock
that ticks only on certain subevents.
-}
module FRP.Rhine.Clock.Select where

-- base
import Data.Data

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Schedule

-- dunai
import Data.MonadicStreamFunction.Async (concatS)

-- base
import Data.Maybe (catMaybes, maybeToList)

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

instance
  ( Monad m
  , Clock m cl
  , Data (Time cl)
  , Data a
  ) =>
  Clock m (SelectClock cl a)
  where
  type Time (SelectClock cl a) = Time cl
  type Tag (SelectClock cl a) = a
  initClock SelectClock {..} = do
    (runningClock, initialTime) <- initClock mainClock
    let
      runningSelectClock = filterS $ proc _ -> do
        (time, tag) <- runningClock -< ()
        returnA -< (time,) <$> select tag
    return (runningSelectClock, initialTime)

instance GetClockProxy (SelectClock cl a)

{- | A universal schedule for two subclocks of the same main clock.
   The main clock must be a 'Semigroup' (e.g. a singleton).
-}
schedSelectClocks ::
  ( Monad m
  , Semigroup cl
  , Clock m cl
  , Data (Time cl)
  , Data (Tag cl)
  , Data a
  , Data b
  ) =>
  Schedule m (SelectClock cl a) (SelectClock cl b)
schedSelectClocks = Schedule {..}
  where
    initSchedule subClock1 subClock2 = do
      (runningClock, initialTime) <-
        initClock $
          mainClock subClock1 <> mainClock subClock2
      let
        runningSelectClocks = concatS $ proc _ -> do
          (time, tag) <- runningClock -< ()
          returnA
            -<
              catMaybes
                [ (time,) . Left <$> select subClock1 tag
                , (time,) . Right <$> select subClock2 tag
                ]
      return (runningSelectClocks, initialTime)

-- | A universal schedule for a subclock and its main clock.
schedSelectClockAndMain ::
  ( Monad m
  , Semigroup cl
  , Clock m cl
  , Data (Time cl)
  , Data (Tag cl)
  , Data a
  ) =>
  Schedule m cl (SelectClock cl a)
schedSelectClockAndMain = Schedule {..}
  where
    initSchedule mainClock' SelectClock {..} = do
      (runningClock, initialTime) <-
        initClock $
          mainClock' <> mainClock
      let
        runningSelectClock = concatS $ proc _ -> do
          (time, tag) <- runningClock -< ()
          returnA
            -<
              catMaybes
                [ Just (time, Left tag)
                , (time,) . Right <$> select tag
                ]
      return (runningSelectClock, initialTime)

{- | Helper function that runs an 'MSF' with 'Maybe' output
   until it returns a value.
-}
filterS :: (Monad m, Data b) => MSF m () (Maybe b) -> MSF m () b
filterS = concatS . (>>> arr maybeToList)
