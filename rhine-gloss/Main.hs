{- | Example application for the @gloss@ wrapper. -}

{-# LANGUAGE TypeFamilies #-}

-- base
import qualified Control.Category as Category
import Data.Maybe (maybeToList)

-- rhine-gloss
import FRP.Rhine.Gloss

-- | Calculate a gear wheel rotated by a certain angle.
gears :: Float -> Picture
gears angle = color green $ pictures
  $ circleSolid 60
  : map (rotate angle) [ rotate (45 * n) $ rectangleSolid 20 150 | n <- [0..3] ]

-- | Rotate the gear with a constant angular velocity.
--   Disregards all events.
sim :: Monad m => BehaviourF m Float [Event] Picture
sim = timeInfoOf sinceInit >>> arr (* 50) >>> arr gears

main :: IO ()
main = do
  putStrLn "Please choose between the pure (1), the pure combined, and the IO backend (2):"
  n <- readLn
  case n of
    1 -> flowGloss defaultSettings $ currentEvent >>> arr maybeToList >>> sim
    2 -> flowGlossCombined defaultSettings pureRhine
    3 -> flowGlossIO defaultSettings ioRhine

-- | Run the gears simulation with the pure backend
pureRhine = tagS @@ glossEventClock >-- collect -@- glossSchedule --> sim >-> arrMCl paintAll @@ glossSimulationClock

-- | Run the gears simulation with the 'IO' backend
ioRhine = tagS @@ GlossEventClockIO >-- collect -@- glossConcurrently --> sim >-> arrMCl paintAllIO @@ GlossSimClockIO
