{-# LANGUAGE Arrows           #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}


-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Schedule.Concurrently

-- | Calculates and prints the rounding errors that accumulate
--   when calculating the time since the start of the simulation
--   via an Euler integral.
showRoundingError
  :: Diff (TimeDomainOf cl) ~ Double
  => String -> SyncSF IO cl () ()
showRoundingError clName = proc () -> do
  correct   <- timeInfoOf sinceStart -< ()
  simulated <- arr_ 1 >>> integral   -< ()
  liftS putStrLn -<
    "Clock " ++ clName
    ++ " ticks at time "     ++ show correct
    ++ " and simulates "     ++ show simulated
    ++ " => rounding error " ++ show (correct - simulated)

-- | Show the rounding error for the 1000 milliseconds clock.
showREMS1000 :: SyncSF IO (Millisecond 1000) () ()
showREMS1000 = showRoundingError "Millisecond 1000"

-- | Show the rounding error for the 350 milliseconds clock.
showREMS350 :: SyncSF IO (Millisecond 350) () ()
showREMS350 = showRoundingError "Millisecond 350"

-- | The main program runs both synchronous signal functions in parallel,
--   using a concurrent (GHC threads) schedule.
main :: IO ()
main = flow $ showREMS350 @@ waitClock **@ concurrently @** showREMS1000 @@ waitClock
