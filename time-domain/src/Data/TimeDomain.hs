{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
This module defines the 'TimeDomain' class.
Its instances model time,
simulated and realtime.
Several instances such as 'UTCTime', 'Double' and 'Integer' are supplied here.
-}
module Data.TimeDomain (
  module Data.TimeDomain,
  UTCTime,
)
where

-- time
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)

{- |
A time domain is an affine space representing a notion of time,
such as real time, simulated time, steps, or a completely different notion.

Expected laws:

* @(t1 `diffTime` t3) `difference` (t1 `diffTime` t2) = t2 `diffTime` t3@
* @(t `addTime` dt) `diffTime` t = dt@
* @(t `addTime` dt1) `addTime` dt2 = t `addTime` (dt1 `add` dt2)@
-}
class (TimeDifference (Diff time)) => TimeDomain time where
  -- | The type of differences or durations between two timestamps
  type Diff time

  -- | Compute the difference between two timestamps.
  --
  --   Mnemonic: 'diffTime' behaves like the '(-)' operator:
  --
  --   @'diffTime' earlier later = later `'diffTime'` earlier@ is the duration it takes from @earlier@ to @later@.
  diffTime :: time -> time -> Diff time

  -- | Add a time difference to a timestamp.
  addTime :: time -> Diff time -> time

{- | A type of durations, or differences betweens time stamps.

Expected laws:

* `add` is commutative and associative
* @(dt1 `difference` dt2) `add` dt2 = dt1@
-}
class TimeDifference d where
  -- | Calculate the difference between two durations,
  --   compatibly with 'diffTime'.
  difference :: d -> d -> d

  -- | Add two time differences.
  add :: d -> d -> d

-- | Differences between 'UTCTime's are measured in seconds.
instance TimeDomain UTCTime where
  type Diff UTCTime = Double
  diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2
  addTime = flip $ addUTCTime . realToFrac

instance TimeDifference Double where
  difference = (-)
  add = (+)

instance TimeDomain Double where
  type Diff Double = Double
  diffTime = (-)
  addTime = (+)

instance TimeDifference Float where
  difference = (-)
  add = (+)

instance TimeDomain Float where
  type Diff Float = Float
  diffTime = (-)
  addTime = (+)

instance TimeDifference Integer where
  difference = (-)
  add = (+)

instance TimeDomain Integer where
  type Diff Integer = Integer
  diffTime = (-)
  addTime = (+)

instance TimeDifference () where
  difference _ _ = ()
  add _ _ = ()

instance TimeDomain () where
  type Diff () = ()
  diffTime _ _ = ()
  addTime _ _ = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain {fromNumTimeDomain :: a}
  deriving (Num)

instance (Num a) => TimeDifference (NumTimeDomain a) where
  difference = (-)
  add = (+)

instance (Num a) => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
  diffTime = (-)
  addTime = (+)
