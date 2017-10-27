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
-- import Data.MonadicStreamFunction.Async (concatS)
-- TODO dunai 0.1.2

-- base
import Data.Maybe (catMaybes, maybeToList)

-- | A clock that selects certain subevents of type 'a',
--   from the tag of a main clock.
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

-- ** To be ported to @dunai@

concatS :: Monad m => MStream m [b] -> MStream m b
concatS msf = MSF $ \_ -> tick msf []
  where
    tick msf (b:bs) = return (b, MSF $ \_ -> tick msf bs)
    tick msf []     = do
      (bs, msf') <- unMSF msf ()
      tick msf' bs

filterS :: Monad m => MSF m () (Maybe b) -> MSF m () b
filterS = concatS . (>>> arr maybeToList)
