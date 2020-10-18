{- |
Clocks implemented in the 'ScheduleT' monad transformer
can always be scheduled (by construction).
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Schedule.Trans where

-- dunai
import Data.MonadicStreamFunction.InternalCore

-- rhine
import Control.Monad.Schedule
import Control.Monad.Schedule.Class
import FRP.Rhine.Clock
import FRP.Rhine.Schedule


-- * Universal schedule for the 'ScheduleT' monad transformer

-- | Two clocks in the 'ScheduleT' monad transformer
--   can always be canonically scheduled.
--   Indeed, this is the purpose for which 'ScheduleT' was defined.
schedule
  :: ( Monad m
     , Clock (ScheduleT (Diff (Time cl1)) m) cl1
     , Clock (ScheduleT (Diff (Time cl1)) m) cl2
     , Time cl1 ~ Time cl2
     , Ord (Diff (Time cl1))
     , Num (Diff (Time cl1))
     )
  => Schedule (ScheduleT (Diff (Time cl1)) m) cl1 cl2
schedule = Schedule {..}
  where
    initSchedule cl1 cl2 = do
      (runningClock1, initTime) <- initClock cl1
      (runningClock2, _)        <- initClock cl2
      return
        ( runningSchedule cl1 cl2 runningClock1 runningClock2
        , initTime
        )

    -- Combines the two individual running clocks to one running clock.
    runningSchedule
      :: ( Monad m
         , Clock (ScheduleT (Diff (Time cl1)) m) cl1
         , Clock (ScheduleT (Diff (Time cl2)) m) cl2
         , Time cl1 ~ Time cl2
         , Ord (Diff (Time cl1))
         , Num (Diff (Time cl1))
         )
      => cl1 -> cl2
      -> MSF (ScheduleT (Diff (Time cl1)) m) () (Time cl1, Tag cl1)
      -> MSF (ScheduleT (Diff (Time cl1)) m) () (Time cl2, Tag cl2)
      -> MSF (ScheduleT (Diff (Time cl1)) m) () (Time cl1, Either (Tag cl1) (Tag cl2))
    runningSchedule cl1 cl2 rc1 rc2 = MSF $ \_ -> do
      -- Race both clocks against each other
      raceResult <- race (unMSF rc1 ()) (unMSF rc2 ())
      case raceResult of
        -- The first clock ticks first...
        Left  (((time, tag1), rc1'), cont2) -> return
          -- so we can emit its time stamp...
          ( (time, Left tag1)
          -- and continue.
          , runningSchedule cl1 cl2 rc1' (MSF $ const cont2)
          )
        -- The second clock ticks first...
        Right (cont1, ((time, tag2), rc2')) -> return
          -- so we can emit its time stamp...
          ( (time, Right tag2)
          -- and continue.
          , runningSchedule cl1 cl2 (MSF $ const cont1) rc2'
          )
