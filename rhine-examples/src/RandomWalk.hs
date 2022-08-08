{- | Simulation of a random walk.

The internal state is a point in 2D space.
Every millisecond, a unit step is taken in a random direction along either the X or Y axis.
The current position and the distance to the origin is shown, as well as the position and distance to a saved point.
(A point can be saved by pressing enter.)

This mainly exists to test the 'feedbackRhine' construct.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- random
import System.Random

-- simple-affine-space
import Data.Vector2

-- rhine
import FRP.Rhine

type Point = Vector2 Float

type SimulationClock = Millisecond 1
type DisplayClock = Millisecond 1000
type AppClock = SequentialClock IO StdinClock (SequentialClock IO SimulationClock DisplayClock)

-- | On every newline, show the current point and the local time.
--   Also, forward the current point so it can be saved.
keyboard :: ClSF IO StdinClock ((), Point) Point
keyboard = proc ((), currentPoint) -> do
  arrMCl putStrLn -< "Saving: " ++ show currentPoint
  debugLocalTime -< ()
  returnA -< currentPoint

-- | Every millisecond, go one step up, down, right or left.
--   Also, forward the current point when it was marked by the last newline.
simulation :: ClSF IO SimulationClock Point (Point, Point)
simulation = feedback zeroVector $ proc (savedPoint, lastPoint) -> do
  direction <- constMCl $ randomRIO (0, 3 :: Int) -< ()
  let
    shift = case direction of
      0 -> vector2 (-1) 0
      1 -> vector2 1 0
      2 -> vector2 0 (-1)
      3 -> vector2 0 1
      _ -> error "simulation: Internal error"
    nextPoint = lastPoint ^+^ shift
  returnA -< ((savedPoint, nextPoint), nextPoint)

-- | Every second, display the current simulated point and the point saved by the keyboard,
--   together with the distances from current point to origin and saved point, respectively.
display :: ClSF IO DisplayClock (Point, Point) ((), Point)
display = proc (savedPoint, currentPoint) -> do
  let
    distanceOrigin = norm currentPoint
    distanceSaved = norm $ currentPoint ^-^ savedPoint
  arrMCl putStrLn -<
    "Saved: " ++ show savedPoint
    ++ "\nCurrent: " ++ show currentPoint
    ++ "\nDistance to origin: " ++ show distanceOrigin
    ++ "\nDistance to saved: " ++ show distanceSaved
  returnA -< ((), currentPoint)

-- | A helper to observe the difference between time since clock initialisation and local time
debugLocalTime :: BehaviourF IO UTCTime a a
debugLocalTime = proc a -> do
  sinceInit_ <- sinceInitS -< ()
  sinceStart_ <- sinceStart -< ()
  arrMCl putStrLn -<"since init: " ++ show sinceInit_ ++ "\nsince start: " ++ show sinceStart_
  returnA -< a

-- | Wire together all components
mainRhine :: Rhine IO AppClock () ()
mainRhine = feedbackRhine (debugLocalTime ^->> keepLast zeroVector) $
  keyboard @@ StdinClock >-- keepLast zeroVector -@- concurrently --> simulation @@ waitClock >-- keepLast (zeroVector, zeroVector) -@- scheduleMillisecond --> display @@ waitClock

-- | Execute the main Rhine
main :: IO ()
main = flow mainRhine
