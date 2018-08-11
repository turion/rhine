-- | A visualisation of different kinds of interpolating 'ResamplingBuffer's.
--   A random signal of 2-dimensional positions is produced
--   and resampled (parallely) in 4 different ways, namely:
--   * No interpolation (zero-order hold)
--   * Linear interpolation
--   * Cubic interpolation
--   * Sinc (Whittaker-Shannon) interpolation
--   By using the Gloss backend, the original signal is displayed
--   in the top left corner of the screen,
--   followed by the interpolated signals in clockwise order. 

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- base
import Control.Monad.IO.Class

-- random
import System.Random (randomRIO)

-- rhine
import Control.Monad.Schedule
import FRP.Rhine
import FRP.Rhine.Clock.Periodic
import FRP.Rhine.ResamplingBuffer.Interpolation
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.Schedule.Trans


-- rhine-gloss
import FRP.Rhine.Gloss

-- dunai
import Data.VectorSpace

-- | Time intervals chosen by fair dice roll. Guaranteed to be random.
type RandomWalkClock = RescaledClock (Periodic [ 300, 500, 200, 700, 400, 900 ]) Float

randomWalkClock :: RandomWalkClock
randomWalkClock = RescaledClock
  { unscaledClock = Periodic
  , rescale       = (* 0.001) . fromIntegral
  }


-- | Produce random velocities and integrate them to random motion.
--   Additionally, a displacement velocity is added to keep the position
--   from moving too far from the origin.
randomWalk :: MonadIO m => ClSF m RandomWalkClock () Point
randomWalk = feedback zeroVector $ proc (_, x) -> do
  [randomVx, randomVy] <- constMCl $ liftIO $ randomRIO (-1, 1) -< ()
  let velocity = (randomVx, randomVy) ^-^ x ^* 1
  x' <- integral -< velocity
  returnA        -< (x', x')

markerShape = circle 10
offset = 200
labelOffset = 20
marker (x, y) theColour label = translate x y $ color theColour $ pictures
  [ markerShape
  , translate 0 labelOffset $ text label
  ]


-- | Display the four signals in the four corners of the window,
--   with different colours and labels.
display (((imgOrig, imgLinear), imgCubic), imgSinc) = pictures
  [ marker imgOrig   white "Original"
  , marker imgLinear red   "Linear"
  , marker imgCubic  green "Cubic"
  , marker imgSinc   blue  "Sinc"
  ]

-- | Resample the random walk in four different ways.
allBuffers
  :: Diff (Time cl) ~ Float
  => ResamplingBuffer m RandomWalkClock cl Point (((Point, Point), Point), Point)
allBuffers = keepLast zeroVector
         &-& linear zeroVector zeroVector
         &-& cubic
         &-& sinc 100 

-- | The main program:
--   1. Create the random walk
--   2. Create the four interpolations
--   3. Display them
main :: IO ()
main = runScheduleIO $ flow
  $ randomWalk @@ randomWalkClock >-- allBuffers -@- schedule --> arr display @@ _
