{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
--{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}

module FRP.Rhine.ClockTree where

-- import Data.Kind

import FRP.Rhine.Clock
import FRP.Rhine.Schedule hiding (LastTime)

-- TODO Possibly have a single TimeDomain as parameter?
-- Yep, tried that, needs TypeInType and still doesn't do what I want
data ClockTopology where
  TLeaf :: (cl :: *) -> ClockTopology
  TSequential :: ClockTopology -> ClockTopology -> ClockTopology
  TParallel :: ClockTopology -> ClockTopology -> ClockTopology
{-
type family Leftmost' (cto :: ClockTopology) :: * where -- :: ClockTopology -> * where
  Leftmost' (TLeaf cl) = cl
  Leftmost' (TSequential cto _) = Leftmost' cto
  Leftmost' (TParallel cto1 cto2) = TParallel (Leftmost' cto1) (Leftmost' cto2) -- TODO Is this right? (We don't have a clock value for that)

type family Rightmost' (cto :: ClockTopology) :: * where -- :: ClockTopology -> *
  Rightmost' (TLeaf cl) = cl
  Rightmost' (TSequential _ cto) = Rightmost' cto
  Rightmost' (TParallel cto1 cto2) = TParallel (Rightmost' cto1) (Rightmost' cto2)
-}

inCl :: ClockTopology -> ClockTopology
inCl (TLeaf cl) = TLeaf cl
inCl (TSequential cto _) = cto
inCl (TParallel cto1 cto2) = TParallel (inCl cto1) (inCl cto2)

outCl :: ClockTopology -> ClockTopology
outCl (TLeaf cl) = TLeaf cl
outCl (TSequential _ cto) = cto
outCl (TParallel cto1 cto2) = TParallel (outCl cto1) (outCl cto2)

-- TODO Possibly add constraints on TimeDomains (and Clock) here? Or somehow up in the topology?
data ClockTree m (cto :: ClockTopology) where
  CLeaf :: (Clock m cl) =>
    cl -> ClockTree m (TLeaf cl)
  CSequential :: (Time (ClockTree m cto1) ~ Time (ClockTree m cto2)) => ClockTree m cto1 -> ClockTree m cto2 -> Schedule m (ClockTree m cto1) (ClockTree m cto2) -> ClockTree m (TSequential cto1 cto2)
  CParallel :: (Time (ClockTree m cto1) ~ Time (ClockTree m cto2)) => ClockTree m cto1 -> ClockTree m cto2 -> Schedule m (ClockTree m cto1) (ClockTree m cto2) -> ClockTree m (TParallel cto1 cto2)

instance Clock m cl => Clock m (ClockTree m (TLeaf cl)) where
  type Time (ClockTree m (TLeaf cl)) = Time cl
  type Tag (ClockTree m (TLeaf cl)) = Tag cl
  initClock (CLeaf cl) = initClock cl

instance (Clock m (ClockTree m cto1), Clock m (ClockTree m cto2)) => Clock m (ClockTree m (TSequential cto1 cto2)) where
  type Time (ClockTree m (TSequential cto1 cto2)) = Time (ClockTree m cto1)
  type Tag (ClockTree m (TSequential cto1 cto2)) = Either (Tag (ClockTree m cto1)) (Tag (ClockTree m cto2))
  initClock (CSequential ct1 ct2 (Schedule sched)) = sched ct1 ct2

instance (Clock m (ClockTree m cto1), Clock m (ClockTree m cto2)) => Clock m (ClockTree m (TParallel cto1 cto2)) where
  type Time (ClockTree m (TParallel cto1 cto2)) = Time (ClockTree m cto1)
  type Tag (ClockTree m (TParallel cto1 cto2)) = Either (Tag (ClockTree m cto1)) (Tag (ClockTree m cto2))
  initClock (CParallel ct1 ct2 (Schedule sched)) = sched ct1 ct2
-- TODO Parallel instance

data LastTime (cto :: ClockTopology) where
  LTLeaf :: Time cl -> LastTime (TLeaf cl)
  LTSequential :: LastTime cto1 -> LastTime cto2 -> LastTime (TSequential cto1 cto2)
  LTParallel :: LastTime cto1 -> LastTime cto2 -> LastTime (TParallel cto1 cto2)

initLastTime :: ClockTree m cto -> Time (ClockTree m cto) -> LastTime cto
--initLastTime :: (ct :: ClockTree m cto) -> Time ct -> LastTime cto
initLastTime (CLeaf _) time = LTLeaf time
initLastTime (CSequential ct1 ct2 _) time = LTSequential (initLastTime ct1 time) (initLastTime ct2 time)
initLastTime (CParallel ct1 ct2 _) time = LTParallel (initLastTime ct1 time) (initLastTime ct2 time)


updateLastTime :: ClockTree m cto -> LastTime cto -> Time (ClockTree m cto) -> Tag (ClockTree m cto) -> LastTime cto
updateLastTime (CLeaf _) (LTLeaf _) time _ = LTLeaf time
updateLastTime (CSequential ct1 _ _) (LTSequential lt1 lt2) time (Left tag) = LTSequential (updateLastTime ct1 lt1 time tag) lt2
updateLastTime (CSequential _ ct2 _) (LTSequential lt1 lt2) time (Right tag) = LTSequential lt1 (updateLastTime ct2 lt2 time tag)
updateLastTime (CParallel ct1 _ _) (LTParallel lt1 lt2) time (Left tag) = LTParallel (updateLastTime ct1 lt1 time tag) lt2
updateLastTime (CParallel _ ct2 _) (LTParallel lt1 lt2) time (Right tag) = LTParallel lt1 (updateLastTime ct2 lt2 time tag)

-- The following doesn't understand certain types for some reason
{-
updateLastTime _ (LTLeaf _) time _ = LTLeaf time
updateLastTime _ (LTSequential lt1 lt2) time (Left tag) = LTSequential (updateLastTime lt1 time tag) lt2
updateLastTime _ (LTSequential lt1 lt2) time (Right tag) = LTSequential lt1 (updateLastTime lt2 time tag)
updateLastTime _ (LTParallel lt1 lt2) time (Left tag) = LTParallel (updateLastTime lt1 time tag) lt2
updateLastTime _ (LTParallel lt1 lt2) time (Right tag) = LTParallel lt1 (updateLastTime lt2 time tag)
-}
