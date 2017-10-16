{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}
module FRP.Rhine.Schedule.Trans where

-- rhine
import Control.Monad.Schedule
import FRP.Rhine


-- * Universal schedule for the 'ScheduleT' monad transformer

-- | Two clocks in the 'ScheduleT' monad transformer
--   can always be canonically scheduled.
--   Indeed, this is the purpose for which 'ScheduleT' was defined.
schedule
  :: ( Monad m
     , Clock (ScheduleT (Diff (TimeDomainOf cl1)) m) cl1
     , Clock (ScheduleT (Diff (TimeDomainOf cl1)) m) cl2
     , TimeDomainOf cl1 ~ TimeDomainOf cl2
     , Ord (Diff (TimeDomainOf cl1))
     , Num (Diff (TimeDomainOf cl1))
     )
  => Schedule (ScheduleT (Diff (TimeDomainOf cl1)) m) cl1 cl2
schedule = Schedule {..}
  where
    startSchedule cl1 cl2 = do
      (runningClock1, initTime) <- startClock cl1
      (runningClock2, _)        <- startClock cl2
      return
        ( runningSchedule cl1 cl2 runningClock1 runningClock2
        , initTime
        )

    -- Combines the two individual running clocks to one running clock.
    runningSchedule
      :: ( Monad m
         , Clock (ScheduleT (Diff (TimeDomainOf cl1)) m) cl1
         , Clock (ScheduleT (Diff (TimeDomainOf cl2)) m) cl2
         , TimeDomainOf cl1 ~ TimeDomainOf cl2
         , Ord (Diff (TimeDomainOf cl1))
         , Num (Diff (TimeDomainOf cl1))
         )
      => cl1 -> cl2
      -> MSF (ScheduleT (Diff (TimeDomainOf cl1)) m) () (TimeDomainOf cl1, Tag cl1)
      -> MSF (ScheduleT (Diff (TimeDomainOf cl1)) m) () (TimeDomainOf cl2, Tag cl2)
      -> MSF (ScheduleT (Diff (TimeDomainOf cl1)) m) () (TimeDomainOf cl1, Either (Tag cl1) (Tag cl2))
    runningSchedule cl1 cl2 rc1 rc2 = MSF $ \_ -> do
      -- Race both clocks against each other
      raceResult <- race (unMSF rc1 ()) (unMSF rc2 ())
      case raceResult of
        -- The first clock ticks first...
        Left  (((td, tag1), rc1'), cont2) -> return
          -- so we can emit its time stamp...
          ( (td, Left tag1)
          -- and continue.
          , runningSchedule cl1 cl2 rc1' (MSF $ const cont2)
          )
        -- The second clock ticks first...
        Right (cont1, ((td, tag2), rc2')) -> return
          -- so we can emit its time stamp...
          ( (td, Right tag2)
          -- and continue.
          , runningSchedule cl1 cl2 (MSF $ const cont1) rc2'
          )
