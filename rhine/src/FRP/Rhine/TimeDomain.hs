{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module FRP.Rhine.TimeDomain
  ( module FRP.Rhine.TimeDomain
  , UTCTime
  )
  where

-- time
import Data.Time.Clock (UTCTime, diffUTCTime)

-- dunai
import Data.VectorSpace.Specific ()


-- | A time domain is an affine space representing a notion of time,
--   such as real time, simulated time, steps, or a completely different notion.
class TimeDomain time where
  type Diff time
  diffTime :: time -> time -> Diff time


instance TimeDomain UTCTime where
  type Diff UTCTime = Double
  diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2

instance TimeDomain Double where
  type Diff Double = Double
  diffTime = (-)

instance TimeDomain Float where
  type Diff Float = Float
  diffTime = (-)

instance TimeDomain Integer where
  type Diff Integer = Integer
  diffTime          = (-)

instance TimeDomain () where
  type Diff () = ()
  diffTime _ _ = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain { fromNumTimeDomain :: a }
  deriving Num

instance Num a => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
  diffTime = (-)
