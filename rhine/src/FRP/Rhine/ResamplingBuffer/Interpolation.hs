{-# LANGUAGE Arrows       #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.ResamplingBuffer.Interpolation where


-- dunai
import Data.VectorSpace

-- rhine
import FRP.Rhine
import FRP.Rhine.ResamplingBuffer.KeepLast

-- | A simple linear interpolation based on the last calculated position and velocity.
linear
  :: ( Monad m, Clock m cl1, Clock m cl2
     , VectorSpace v
     , Groundfield v ~ Diff (Time cl1)
     , Groundfield v ~ Diff (Time cl2)
     )
  => v -- ^ The initial velocity (derivative of the signal)
  -> v -- ^ The initial position
  -> ResamplingBuffer m cl1 cl2 v v
linear initVelocity initPosition
  =    (derivativeFrom initPosition &&& clId) &&& timeInfoOf sinceInit
  ^->> keepLast ((initVelocity, initPosition), 0)
  >>-^ proc ((velocity, lastPosition), sinceInit1) -> do
    sinceInit2 <- timeInfoOf sinceInit -< ()
    let diff = sinceInit2 - sinceInit1
    returnA -< lastPosition ^+^ velocity ^* diff
