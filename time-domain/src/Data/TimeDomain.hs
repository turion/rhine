{-# LANGUAGE DeriveTraversable #-}
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

-- | Differences between 'UTCTime's are measured in Secondss.
instance TimeDomain UTCTime where
  type Diff UTCTime = Seconds Double

instance RightAction (Seconds Double) UTCTime where
  actRight = flip $ addUTCTime . realToFrac . getSeconds

instance RightTorsor (Seconds Double) UTCTime where
  differenceRight t1 t2 = Seconds $ realToFrac $ diffUTCTime t1 t2

instance TimeDifference () where
  zero = ()
  difference _ _ = ()
  add _ _ = ()

instance TimeDomain () where
  type Diff () = ()

{- | Any 'Num' can be wrapped to form a 'TimeDomain'.

The number 1 is interpreted as one second.
-}
newtype Seconds a = Seconds {getSeconds :: a}
  deriving newtype (Show, Read, Eq, Ord, Num, Fractional, Floating, Real, RealFrac, RealFloat, Enum, Bounded, Integral)
  deriving stock (Functor, Foldable, Traversable)

deriving via Sum (Seconds a) instance (Num a) => Semigroup (Seconds a)
deriving via Sum (Seconds a) instance (Num a) => Monoid (Seconds a)
deriving via Sum (Seconds a) instance (Num a) => Group (Seconds a)
instance (Num a) => TimeDifference (Seconds a)

-- I would have thought I shouldn't be needing this, but it seems to overlap with the Zip instance for some reason.
instance {-# OVERLAPPING #-} (Num a) => RightAction (Seconds a) (Seconds a) where
  actRight = (+)

instance (Num a) => RightTorsor (Seconds a) (Seconds a) where
  differenceRight = (-)

instance (Num a) => TimeDomain (Seconds a) where
  type Diff (Seconds a) = Seconds a

-- | Scale time differences by a factor.
instance (Num a) => RightAction a (Seconds a) where
  actRight seconds factor = fmap (* factor) seconds

-- | Compute the quotient of two time differences.
instance (Fractional a) => RightTorsor a (Seconds a) where
  differenceRight (Seconds secondsOrig) (Seconds secondsScaled) = secondsOrig / secondsScaled
