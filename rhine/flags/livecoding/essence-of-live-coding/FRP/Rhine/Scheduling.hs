module FRP.Rhine.Scheduling where


-- base
import Data.List.NonEmpty

-- monad-schedule
import Control.Monad.Schedule.Class
import Data.MonadicStreamFunction

scheduleList :: (Monad m, MonadSchedule m) => NonEmpty (MSF m a b) -> MSF m a (NonEmpty b)
scheduleList (Cell state1_ step1 :| [Cell state2_ step2]) = Cell (state1_, state2_) $ \(state1, state2) a -> do
  result <- race (step1 state1 a) (step2 state2 a)
  case result of
    Left thing ->_
    Right thing ->_

  {-MSF $ \a -> do
      let bsAndConts = flip unMSF a <$> msfs
      (done, running) <- schedule (N.head bsAndConts :| N.tail bsAndConts ++ running)
      let (bs, dones) = N.unzip done
      return (bs, scheduleList' dones running)-}
scheduleList _ = error "scheduleList: Only implemented for two cells"
