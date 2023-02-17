{-# LANGUAGE TypeFamilies #-}

-- | Example application for the @gloss@ wrapper.
module Main where

-- base
import Data.Maybe (maybeToList)

-- rhine-gloss
import FRP.Rhine.Gloss

-- | Calculate a gear wheel rotated by a certain angle.
gears :: Float -> Picture
gears angle =
  color green $
    pictures $
      circleSolid 60
        : [rotate (angle + 45 * n) $ rectangleSolid 20 150 | n <- [0 .. 3]]

{- | Rotate the gear with a constant angular velocity.
   Disregards all events.
-}
sim :: Monad m => BehaviourF m Float [Event] Picture
sim = timeInfoOf sinceInit >>> arr (* 50) >>> arr gears

main :: IO ()
main = do
  putStrLn "Please choose between the pure (1), the pure combined (2), and the IO backend (3):"
  n <- readLn
  case n of
    1 -> flowGloss defaultSettings pureClSF
    2 -> flowGlossCombined defaultSettings pureRhine
    3 -> flowGlossIO defaultSettings ioRhine
    _ -> error "Invalid input"

-- | Run the gears simulation with the pure backend synchronously.
pureClSF = currentEvent >>> arr maybeToList >>> sim

{- | Run the gears simulation with the pure backend with two subsystems,
   one at the rate of events, one at the rate of simulation.
-}
pureRhine = tagS @@ glossEventClock >-- collect -@- glossSchedule --> sim >-> arrMCl paintAll @@ glossSimulationClock

-- | Run the gears simulation with the 'IO' backend.
ioRhine = tagS @@ GlossEventClockIO >-- collect -@- glossConcurrently --> sim >-> arrMCl paintAllIO @@ GlossSimClockIO
