{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}

-- transformers
import Control.Monad.IO.Class

-- automaton
import Data.Automaton.Schedule (SkipT, runSkipTWith)

-- rhine

import Control.Concurrent (threadDelay)
import FRP.Rhine

type MyClock = Periodic '[500, 1000]

everyNowAndThen :: (Monad m) => ClSF m MyClock arbitrary String
everyNowAndThen =
  sinceInitS >>> proc time ->
    returnA -< unwords ["It's now", show time, "o'clock."]

mainRhine :: (MonadIO m) => Rhine (SkipT m) MyClock () ()
mainRhine = everyNowAndThen >-> arrMCl (liftIO . putStrLn) @@ Periodic

main :: IO ()
main = runSkipTWith (threadDelay 1000) $ flow mainRhine
