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
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

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

  -- | The origin of the time domain.
  --
  --   All timestamps can be understood as durations relative to this reference point.
  --
  --   For example, 'UTCTime' uses the Unix epoch (1970-01-01 00:00:00 UTC),
  --   while numeric instances use @0@.
  epoch :: time

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

* @dt `add` 'zero' = dt@ and @dt `difference` 'zero' = dt@
* `add` is commutative and associative
* @(dt1 `difference` dt2) `add` dt2 = dt1@
-}
class TimeDifference d where
  -- | The zero duration, i.e. no time has passed.
  --   Additive identity for 'difference' and 'add'.
  zero :: d

  -- | Calculate the difference between two durations,
  --   compatibly with 'diffTime'.
  difference :: d -> d -> d

  -- | Add two time differences.
  add :: d -> d -> d

-- | Differences between 'UTCTime's are measured in seconds.
instance TimeDomain UTCTime where
  type Diff UTCTime = Double
  epoch = posixSecondsToUTCTime 0
  diffTime t1 t2 = realToFrac $ diffUTCTime t1 t2
  addTime = flip $ addUTCTime . realToFrac

instance TimeDifference Double where
  zero = 0
  difference = (-)
  add = (+)

instance TimeDomain Double where
  type Diff Double = Double
  epoch = 0
  diffTime = (-)
  addTime = (+)

instance TimeDifference Float where
  zero = 0
  difference = (-)
  add = (+)

instance TimeDomain Float where
  type Diff Float = Float
  epoch = 0
  diffTime = (-)
  addTime = (+)

instance TimeDifference Integer where
  zero = 0
  difference = (-)
  add = (+)

instance TimeDomain Integer where
  type Diff Integer = Integer
  epoch = 0
  diffTime = (-)
  addTime = (+)

instance TimeDifference () where
  zero = ()
  difference _ _ = ()
  add _ _ = ()

instance TimeDomain () where
  type Diff () = ()
  epoch = ()
  diffTime _ _ = ()
  addTime _ _ = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain {fromNumTimeDomain :: a}
  deriving (Num)

instance (Num a) => TimeDifference (NumTimeDomain a) where
  zero = 0
  difference = (-)
  add = (+)

instance (Num a) => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
  epoch = 0
  diffTime = (-)
  addTime = (+)
