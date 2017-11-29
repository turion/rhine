{-# LANGUAGE Arrows       #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.ResamplingBuffer.Interpolation where


-- dunai
import Data.VectorSpace

-- rhine
import FRP.Rhine
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.ResamplingBuffer.Util

-- | A simple linear interpolation based on the last calculated position and velocity.
linear
  :: ( Monad m, Clock m cl1, Clock m cl2
     , VectorSpace v
     , Groundfield v ~ Diff (TimeDomainOf cl1)
     , Groundfield v ~ Diff (TimeDomainOf cl2)
     )
  => v -- ^ The initial velocity (derivative of the signal)
  -> v -- ^ The initial position
  -> ResamplingBuffer m cl1 cl2 v v
linear initVelocity initPosition
  =    (derivativeFrom initPosition &&& syncId) &&& timeInfoOf sinceStart
  ^->> keepLast ((initVelocity, initPosition), 0)
  >>-^ proc ((velocity, lastPosition), sinceStart1) -> do
    sinceStart2 <- timeInfoOf sinceStart -< ()
    let diff = sinceStart2 - sinceStart1
    returnA -< lastPosition ^+^ velocity ^* diff
