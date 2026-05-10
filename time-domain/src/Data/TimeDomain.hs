{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- base
import Data.Monoid (Sum (..))

-- time
import Data.Time.Clock (UTCTime, addUTCTime, diffUTCTime)

-- groups
import Data.Group (Group (..))

-- changeset
import Data.Monoid.RightAction (RightAction (actRight), RightTorsor (differenceRight))

{- |
A time domain is an affine space representing a notion of time,
such as real time, simulated time, steps, or a completely different notion.

Expected laws:

* @(t1 `diffTime` t3) `difference` (t1 `diffTime` t2) = t2 `diffTime` t3@
* @(t `addTime` dt) `diffTime` t = dt@
* @(t `addTime` dt1) `addTime` dt2 = t `addTime` (dt1 `add` dt2)@
-}
class (RightAction (Diff time) time, RightTorsor (Diff time) time, TimeDifference (Diff time)) => TimeDomain time where
  -- | The type of differences or durations between two timestamps
  type Diff time

  {- | Compute the difference between two timestamps.

  Mnemonic: 'diffTime' behaves like the '(-)' operator:

  @'diffTime' earlier later = later `'diffTime'` earlier@ is the duration it takes from @earlier@ to @later@.
  -}
  diffTime :: time -> time -> Diff time
  diffTime = differenceRight

  -- | Add a time difference to a timestamp.
  addTime :: time -> Diff time -> time
  addTime = actRight

{- | A type of durations, or differences betweens time stamps.

Expected laws:

* @dt `add` 'zero' = dt@ and @dt `difference` 'zero' = dt@
* `add` is commutative and associative
* @(dt1 `difference` dt2) `add` dt2 = dt1@
-}
class (Group d) => TimeDifference d where
  {- | The zero duration, i.e. no time has passed.
  Additive identity for 'difference' and 'add'.
  -}
  zero :: d
  zero = mempty

  {- | Calculate the difference between two durations,
  compatibly with 'diffTime'.
  -}
  difference :: d -> d -> d
  difference = (~~)

  -- | Add two time differences.
  add :: d -> d -> d
  add = (<>)

-- | Differences between 'UTCTime's are measured in seconds.
instance TimeDomain UTCTime where
  type Diff UTCTime = Double

instance RightAction Double UTCTime where
  actRight = flip $ addUTCTime . realToFrac

instance RightTorsor Double UTCTime where
  differenceRight t1 t2 = realToFrac $ diffUTCTime t1 t2

deriving via Sum Double instance Semigroup Double
deriving via Sum Double instance Monoid Double
deriving via Sum Double instance Group Double
deriving via NumTimeDomain Double instance TimeDifference Double

instance RightAction Double Double where
  actRight = (+)
instance RightTorsor Double Double where
  differenceRight = (-)

instance TimeDomain Double where
  type Diff Double = Double

deriving via Sum Float instance Semigroup Float
deriving via Sum Float instance Monoid Float
deriving via Sum Float instance Group Float
deriving via NumTimeDomain Float instance TimeDifference Float

instance RightAction Float Float where
  actRight = (+)
instance RightTorsor Float Float where
  differenceRight = (-)

instance TimeDomain Float where
  type Diff Float = Float

deriving via Sum Integer instance Semigroup Integer
deriving via Sum Integer instance Monoid Integer
deriving via Sum Integer instance Group Integer
deriving via NumTimeDomain Integer instance TimeDifference Integer

instance RightAction Integer Integer where
  actRight = (+)
instance RightTorsor Integer Integer where
  differenceRight = (-)

instance TimeDomain Integer where
  type Diff Integer = Integer

instance TimeDifference () where
  zero = ()
  difference _ _ = ()
  add _ _ = ()

instance TimeDomain () where
  type Diff () = ()

-- | Any 'Num' can be wrapped to form a 'TimeDomain'.
newtype NumTimeDomain a = NumTimeDomain {fromNumTimeDomain :: a}
  deriving (Num)

deriving via Sum (NumTimeDomain a) instance (Num a) => Semigroup (NumTimeDomain a)
deriving via Sum (NumTimeDomain a) instance (Num a) => Monoid (NumTimeDomain a)
deriving via Sum (NumTimeDomain a) instance (Num a) => Group (NumTimeDomain a)
instance (Num a) => TimeDifference (NumTimeDomain a)

-- I would have thought I shouldn't be needing this, but it seems to overlap with the Zip instance for some reason.
instance {-# OVERLAPPING #-} (Num a) => RightAction (NumTimeDomain a) (NumTimeDomain a) where
  actRight = (+)

instance (Num a) => RightTorsor (NumTimeDomain a) (NumTimeDomain a) where
  differenceRight = (-)

instance (Num a) => TimeDomain (NumTimeDomain a) where
  type Diff (NumTimeDomain a) = NumTimeDomain a
