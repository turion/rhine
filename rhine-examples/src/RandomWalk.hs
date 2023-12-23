{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

{- | Simulation of a random walk.

The internal state is a point in 2D space.
Every millisecond, a unit step is taken in a random direction along either the X or Y axis.
The current position and the distance to the origin is shown, as well as the position and distance to a saved point.
(A point can be saved by pressing enter.)
-}
module Main where

-- random
import System.Random

-- simple-affine-space
import Data.Vector2

-- rhine
import FRP.Rhine hiding (Rhine, flow, sn)
import FRP.Rhine.Rhine.Free
import FRP.Rhine.SN.Free

type Point = Vector2 Float

type SimulationClock = Millisecond 1
type DisplayClock = Millisecond 1000
type AppClock = '[SimulationClock, StdinClock, DisplayClock]

{- | On every newline, show the current point and the local time.
   Also, forward the current point so it can be saved.
-}
keyboard :: ClSF IO StdinClock Point Point
keyboard = proc currentPoint -> do
  arrMCl putStrLn -< "Saving: " ++ show currentPoint
  debugLocalTime -< ()
  returnA -< currentPoint

-- | Every millisecond, go one step up, down, right or left.
simulation :: ClSF IO SimulationClock () Point
simulation = feedback zeroVector $ proc ((), lastPoint) -> do
  direction <- constMCl $ randomRIO (0, 3 :: Int) -< ()
  let
    shift = case direction of
      0 -> vector2 (-1) 0
      1 -> vector2 1 0
      2 -> vector2 0 (-1)
      3 -> vector2 0 1
      _ -> error "simulation: Internal error"
    nextPoint = lastPoint ^+^ shift
  returnA -< (nextPoint, nextPoint)

{- | Every second, display the current simulated point and the point saved by the keyboard,
   together with the distances from current point to origin and saved point, respectively.
-}
display :: ClSF IO DisplayClock (Point, Point) ()
display = proc (savedPoint, currentPoint) -> do
  let
    distanceOrigin = norm currentPoint
    distanceSaved = norm $ currentPoint ^-^ savedPoint
  arrMCl putStrLn
    -<
      unlines
        [ "Saved: " ++ show savedPoint
        , "Current: " ++ show currentPoint
        , "Distance to origin: " ++ show distanceOrigin
        , "Distance to saved: " ++ show distanceSaved
        ]

-- | A helper to observe the difference between time since clock initialisation and local time
debugLocalTime :: BehaviourF IO UTCTime a a
debugLocalTime = proc a -> do
  sinceInit_ <- sinceInitS -< ()
  sinceStart_ <- sinceStart -< ()
  arrMCl putStrLn -< "since init: " ++ show sinceInit_ ++ "\nsince first local tick: " ++ show sinceStart_
  returnA -< a

-- | In this example, we will always zero-order resample, that is, by keeping the last value
resample ::
  ( HasClocksOrdered clA clB cls
  , Monad m
  ) =>
  FreeSN m cls (At clA Point) (At clB Point)
resample = resampling $ keepLast zeroVector

-- | Wire together all components
mainRhine :: Rhine IO UTCTime AppClock () ()
mainRhine =
  Rhine
    -- The order of the clocks matters!
    -- Since we are using the `simulation` first, we need to list its clock first.
    { clocks = waitClock .:. StdinClock .:. waitClock .:. cnil
    , sn = proc () -> do
        currentPoint <- synchronous simulation -< pure ()
        savedPoint <- resample <<< synchronous keyboard <<< resample -< currentPoint
        currentPointDisplay <- resample -< currentPoint
        synchronous display -< (,) <$> savedPoint <*> currentPointDisplay
        returnA -< ()
    }

-- | Execute the main Rhine
main :: IO ()
main = flow mainRhine
