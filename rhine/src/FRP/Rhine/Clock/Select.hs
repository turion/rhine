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
import Data.Semigroup

-- | A clock that selects certain subevents of type 'a',
--   from the tag of a main clock.
data SelectClock cl a = SelectClock
  { mainClock :: cl -- ^ The main clock
  -- | Return 'Nothing' if no tick of the subclock is required,
  --   or 'Just a' if the subclock should tick, with tag 'a'.
  , select    :: Tag cl -> Maybe a
  }


instance (Monad m, Clock m cl) => Clock m (SelectClock cl a) where
  type Time (SelectClock cl a) = Time cl
  type Tag  (SelectClock cl a) = a
  initClock SelectClock {..} = do
    (runningClock, initialTime) <- initClock mainClock
    let
      runningSelectClock = filterS $ proc _ -> do
        (time, tag) <- runningClock -< ()
        returnA                     -< (time, ) <$> select tag
    return (runningSelectClock, initialTime)


-- | A universal schedule for two subclocks of the same main clock.
--   The main clock must be a Semigroup (e.g. a singleton).
schedSelectClocks
  :: (Monad m, Semigroup cl, Clock m cl)
  => Schedule m (SelectClock cl a) (SelectClock cl b)
schedSelectClocks = Schedule {..}
  where
    initSchedule subClock1 subClock2 = do
      (runningClock, initialTime) <- initClock
        $ mainClock subClock1 <> mainClock subClock2
      let
        runningSelectClocks = concatS $ proc _ -> do
          (time, tag) <- runningClock -< ()
          returnA                     -< catMaybes
            [ (time, ) . Left  <$> select subClock1 tag
            , (time, ) . Right <$> select subClock2 tag ]
      return (runningSelectClocks, initialTime)

-- | A universal schedule for a subclock and its main clock.
schedSelectClockAndMain
  :: (Monad m, Semigroup cl, Clock m cl)
  => Schedule m cl (SelectClock cl a)
schedSelectClockAndMain = Schedule {..}
  where
    initSchedule mainClock' SelectClock {..} = do
      (runningClock, initialTime) <- initClock
        $ mainClock' <> mainClock
      let
        runningSelectClock = concatS $ proc _ -> do
          (time, tag) <- runningClock -< ()
          returnA                     -< catMaybes
            [ Just (time, Left tag)
            , (time, ) . Right <$> select tag ]
      return (runningSelectClock, initialTime)


-- | Helper function that runs an 'MSF' with 'Maybe' output
--   until it returns a value.
filterS :: Monad m => MSF m () (Maybe b) -> MSF m () b
filterS = concatS . (>>> arr maybeToList)
