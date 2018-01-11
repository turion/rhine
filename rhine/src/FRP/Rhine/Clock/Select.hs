{- | In the Rhine philosophy, _event sources are clocks_.
Often, we want to extract certain subevents from event sources,
e.g. single out only keystrokes from all input device events.
This module provides a general purpose selection clock
that ticks only on certain subevents.
-}
{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
module FRP.Rhine.Clock.Select where

-- rhine
import FRP.Rhine

-- dunai
import Data.MonadicStreamFunction.Async (concatS)

-- base
import Data.Maybe (catMaybes, maybeToList)

-- | A clock that selects certain subevents of type 'a',
--   from the tag of a main clock.
--
--   If two 'SelectClock's would tick on the same type of subevents,
--   but should not have the same type,
--   one should @newtype@ the subevent.
data SelectClock cl a = SelectClock
  { mainClock :: cl -- ^ The main clock
  -- | Return 'Nothing' if no tick of the subclock is required,
  --   or 'Just a' if the subclock should tick, with tag 'a'.
  , select    :: Tag cl -> Maybe a
  }


instance (Monad m, Clock m cl) => Clock m (SelectClock cl a) where
  type TimeDomainOf (SelectClock cl a) = TimeDomainOf cl
  type Tag          (SelectClock cl a) = a
  startClock SelectClock {..} = do
    (runningClock, initialTime) <- startClock mainClock
    let
      runningSelectClock = filterS $ proc _ -> do
        (time, tag) <- runningClock -< ()
        returnA                     -< (time, ) <$> select tag
    return (runningSelectClock, initialTime)


-- | A universal schedule for two subclocks of the same main clock.
--   The main clock must be a monoid (e.g. a singleton).
schedSelectClocks
  :: (Monad m, Monoid cl, Clock m cl)
  => Schedule m (SelectClock cl a) (SelectClock cl b)
schedSelectClocks = Schedule {..}
  where
    startSchedule subClock1 subClock2 = do
      (runningClock, initialTime) <- startClock
        $ mainClock subClock1 `mappend` mainClock subClock2
      let
        runningSelectClocks = concatS $ proc _ -> do
          (time, tag) <- runningClock -< ()
          returnA                     -< catMaybes
            [ (time, ) . Left  <$> select subClock1 tag
            , (time, ) . Right <$> select subClock2 tag ]
      return (runningSelectClocks, initialTime)


-- | Helper function that runs an 'MSF' with 'Maybe' output
--   until it returns a value.
filterS :: Monad m => MSF m () (Maybe b) -> MSF m () b
filterS = concatS . (>>> arr maybeToList)
