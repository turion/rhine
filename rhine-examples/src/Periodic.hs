{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}

-- transformers
import Control.Monad.IO.Class

-- automaton
import Data.Automaton.Schedule (YieldT, runYieldTWith)

-- rhine
import FRP.Rhine
import Control.Concurrent (threadDelay)

type MyClock = Periodic '[500, 1000]

everyNowAndThen :: (Monad m) => ClSF m MyClock arbitrary String
everyNowAndThen =
  sinceInitS >>> proc time ->
    returnA -< unwords ["It's now", show time, "o'clock."]

mainRhine :: (MonadIO m) => Rhine (YieldT m) MyClock () ()
mainRhine = everyNowAndThen >-> arrMCl (liftIO . putStrLn) @@ Periodic

main :: IO ()
main = runYieldTWith (threadDelay 1000) $ flow mainRhine
