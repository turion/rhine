{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}

-- base
import Control.Monad.IO.Class

-- automaton
import Data.Automaton.Schedule.Trans (ScheduleT, runScheduleIO)

-- rhine
import FRP.Rhine

type MyClock = Periodic '[500, 1000]

everyNowAndThen :: (Monad m) => ClSF m MyClock arbitrary String
everyNowAndThen =
  sinceInitS >>> proc time ->
    returnA -< unwords ["It's now", show time, "o'clock."]

mainRhine :: (MonadIO m) => Rhine (ScheduleT (Seconds Integer) m) MyClock () ()
mainRhine = everyNowAndThen >-> arrMCl (liftIO . putStrLn) @@ Periodic

main :: IO ()
main = runScheduleIO $ flow mainRhine
