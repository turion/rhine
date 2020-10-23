{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Clock.Proxy where

-- base
import Data.Kind (Type)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Schedule

-- | Witnesses the structure of a clock type,
--   in particular whether 'SequentialClock's or 'ParallelClock's are involved.
data ClockProxy cl where
  LeafProxy
    :: (cl ~ In cl, cl ~ Out cl)
    => ClockProxy cl
  SequentialProxy
    :: ClockProxy cl1
    -> ClockProxy cl2
    -> ClockProxy (SequentialClock cl1 cl2)
  ParallelProxy
    :: ClockProxy clL
    -> ClockProxy clR
    -> ClockProxy (ParallelClock clL clR)

inProxy :: ClockProxy cl -> ClockProxy (In cl)
inProxy LeafProxy = LeafProxy
inProxy (SequentialProxy p1 p2) = inProxy p1
inProxy (ParallelProxy pL pR) = ParallelProxy (inProxy pL) (inProxy pR)

outProxy :: ClockProxy cl -> ClockProxy (Out cl)
outProxy LeafProxy = LeafProxy
outProxy (SequentialProxy p1 p2) = outProxy p2
outProxy (ParallelProxy pL pR) = ParallelProxy (outProxy pL) (outProxy pR)

-- | Return the incoming tag, assuming that the incoming clock is ticked,
--   and 'Nothing' otherwise.
inTag :: ClockProxy cl -> Tag cl -> Maybe (Tag (In cl))
inTag (SequentialProxy p1 _) (Left  tag1) = inTag p1 tag1
inTag (SequentialProxy _  _) (Right _)    = Nothing
inTag (ParallelProxy pL _) (Left  tagL) = Left  <$> inTag pL tagL
inTag (ParallelProxy _ pR) (Right tagR) = Right <$> inTag pR tagR
inTag LeafProxy tag = Just tag

-- | Return the incoming tag, assuming that the outgoing clock is ticked,
--   and 'Nothing' otherwise.
outTag :: ClockProxy cl -> Tag cl -> Maybe (Tag (Out cl))
outTag (SequentialProxy _ _ ) (Left  _)    = Nothing
outTag (SequentialProxy _ p2) (Right tag2) = outTag p2 tag2
outTag (ParallelProxy pL _) (Left  tagL) = Left  <$> outTag pL tagL
outTag (ParallelProxy _ pR) (Right tagR) = Right <$> outTag pR tagR
outTag LeafProxy tag = Just tag

-- TODO Should this be a superclass with default implementation of clocks? But then we have a circular dependency...
-- No we don't, Schedule should not depend on clock (the type).
-- | Clocks should be able to automatically generate a proxy for themselves.
class GetClockProxy cl where
  getClockProxy :: ClockProxy cl

  default getClockProxy
    :: (cl ~ In cl, cl ~ Out cl)
    => ClockProxy cl
  getClockProxy = LeafProxy

instance (GetClockProxy cl1, GetClockProxy cl2) => GetClockProxy (SequentialClock cl1 cl2) where
  getClockProxy = SequentialProxy getClockProxy getClockProxy

instance (GetClockProxy cl1, GetClockProxy cl2) => GetClockProxy (ParallelClock cl1 cl2) where
  getClockProxy = ParallelProxy getClockProxy getClockProxy

instance GetClockProxy cl => GetClockProxy (HoistClock m1 m2 cl)
instance GetClockProxy cl => GetClockProxy (RescaledClock cl time)
instance GetClockProxy cl => GetClockProxy (RescaledClockM m cl time)
instance GetClockProxy cl => GetClockProxy (RescaledClockS m cl time tag)

-- | Extract a clock proxy from a type.
class ToClockProxy a where
  type Cl a :: Type

  toClockProxy :: a -> ClockProxy (Cl a)

  default toClockProxy
    :: GetClockProxy (Cl a)
    => a -> ClockProxy (Cl a)
  toClockProxy _ = getClockProxy
