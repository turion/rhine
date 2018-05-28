{- |
Generate a simple sawtooth signal using exceptions and 'sinceStart'.
-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond

sawtooth = safely $ do
  try $ sinceStart >>> proc time -> do
    throwOn () -< time > 1
    returnA    -< time
  safe sawtooth

main = flow $ sawtooth >-> arrMCl print @@ (waitClock :: Millisecond 200)
