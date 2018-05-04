{-# LANGUAGE Arrows #-}

-- sdl2
import SDL

-- rhine
import FRP.Rhine
import FRP.Rhine.SDL

main :: IO ()
main = flow $ mainSyncSF @@ defaultSDLClock

mainSyncSF :: SyncSF IO SDLClock () ()
mainSyncSF = timeInfoOf (tag &&& sinceStart) >>> proc ((_, screen, _), t) -> do
  n <- arr sin -< fromIntegral t * 0.001
  arrMSync (\(s, n') -> surfaceFillRect s (Just $ Rectangle (P (V2 100 (100 + round (50 * n')))) (V2 100 100)) $ V4 maxBound maxBound maxBound maxBound) -< (screen, n)
