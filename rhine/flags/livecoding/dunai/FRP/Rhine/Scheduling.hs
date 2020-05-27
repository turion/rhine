module FRP.Rhine.Scheduling where

-- dunai
import Data.MonadicStreamFunction.InternalCore (MSF (..))

-- base
import Data.List.NonEmpty

-- monad-schedule
import Control.Monad.Schedule.Class


scheduleList :: (Monad m, MonadSchedule m) => NonEmpty (MSF m a b) -> MSF m a (NonEmpty b)
scheduleList msfs = scheduleList' msfs []
  where
    scheduleList' msfs running = MSF $ \a -> do
      let bsAndConts = flip unMSF a <$> msfs
      (done, running) <- schedule (N.head bsAndConts :| N.tail bsAndConts ++ running)
      let (bs, dones) = N.unzip done
      return (bs, scheduleList' dones running)
