{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module FRP.Rhine.SNGeneralization where
import Data.Kind (Type)
import FRP.Rhine.ClSF
import FRP.Rhine.ResamplingBuffer
import Data.HList
import FRP.Rhine.Reactimation.ClockErasure (eraseClockClSF, eraseClockResBuf)
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Schedule (In, Out)
import Data.Void

-- FIXME Clocked a cl or Clocked cl a?
data Clocked = Clocked Type Type

data SN td (inClocks :: [Clocked]) (internalClocks :: [Type]) (outClocks :: [Clocked]) (m :: Type -> Type) where
  Empty :: SN td '[] '[] '[] m
  Synchronous ::
    (Clock m cl, td ~ Time cl, cl ~ In cl, cl ~ Out cl) =>
    ClSF m cl a b ->
    SN td inClocks internalClocks outClocks m ->
    SN td ('Clocked a cl ': inClocks) (cl ': internalClocks) ('Clocked b cl ': outClocks) m
  Resampling ::
    (td ~ Time cl1, td ~ Time cl2) =>
    ResBuf m cl1 cl2 a b ->
    SN td ('Clocked b cl2 ': inClocks) internalClocks ('Clocked a cl1 ': outClocks) m ->
    SN td inClocks internalClocks outClocks m
  PermuteInClocks ::
    SN td inClocksBefore internalClocks outClocks m ->
    Permutation inClocksBefore inClocksAfter ->
    SN td inClocksAfter internalClocks outClocks m
  PermuteInternalClocks ::
    SN td inClocks internalClocksBefore outClocks m ->
    Permutation internalClocksBefore internalClocksAfter ->
    SN td inClocks internalClocksAfter outClocks m
  PermuteOutClocks ::
    SN td inClocks internalClocks outClocksBefore m ->
    Permutation outClocksBefore outClocksAfter ->
    SN td inClocks internalClocks outClocksAfter m
  DoneIn ::
    SN td ('Clocked () cl ': inClocks) internalClocks outClocks m ->
    SN td inClocks internalClocks outClocks m
  -- TODO Or do we want to be able to erase arbitrary output?
  DoneOut ::
    SN td inClocks internalClocks ('Clocked () cl ': outClocks) m ->
    SN td inClocks internalClocks outClocks m
  -- FIXME get rid of if nil & cons work
  ConcatIn2 ::
    SN td ('Clocked a cl1 ': 'Clocked b cl2 ': inClocks) internalClocks outClocks m ->
    SN td ('Clocked (HList '[a, b]) (Clocks td '[cl1, cl2]) ': inClocks) internalClocks outClocks m
  -- TODO Replicate 3 of these. Not sure this can be refactored to reduce duplication
  ConcatInNil ::
    SN td inClocks internalClocks outClocks m ->
    SN td ('Clocked (HList '[]) (Clocks td '[]) ': inClocks) internalClocks outClocks m
  ConcatInCons ::
    SN td ('Clocked (HList as) (Clocks td cls) ': 'Clocked a cl ': inClocks) internalClocks outClocks m ->
    SN td ('Clocked (HList (a ': as)) (Clocks td (cl ': cls)) ': inClocks) internalClocks outClocks m

data Clocks (td :: Type) (cls :: [Type]) = Clocks (HList cls)

instance TimeDomain td => Clock m (Clocks td '[]) where
  type Time (Clocks td '[]) = td
  type Tag (Clocks td '[]) = Void
  initClock cl = _

instance (TimeDomain td, Clock m cl, Clock m (Clocks td cls), Time cl ~ Time (Clocks td cls)) => Clock m (Clocks td (cl ': cls)) where
  type Time (Clocks td (cl ': cls)) = td
  type Tag (Clocks td (cl ': cls)) = Either (Tag cl) (Tag (Clocks td cls))
  initClock cl = _

data Permutation (before :: [a]) (after :: [a]) where
  Identity :: Permutation as as
  SwapAnd :: Swap before intermediate -> Permutation intermediate after -> Permutation before after

deriving instance (Show (Permutation before after))

data Swap (before :: [a]) (after :: [a]) where
  Here :: Swap (a1 ': a2 ': as) (a2 ': a1 ': as)
  There :: Swap before after -> Swap (a ': before) (a ': after)

deriving instance (Show (Swap before after))

-- data InClocked (inClocks :: [Clocked]) where
--   InNil :: InClocked '[]
--   InCons :: a -> InClocked clas -> InClocked ('Clocked a cl ': clas)

-- data OutClocked (outClocks :: [Clocked]) where
--   OutHere :: a -> OutClocked ('Clocked a cl ': clas)
--   OutThere :: OutClocked clas -> OutClocked ('Clocked a cl ': clas)

data ClockedData (clocked :: [Clocked]) where
  CDLeft :: a -> ClockedData ('Clocked a cl ': clas)
  CDRight :: ClockedData clas -> ClockedData ('Clocked a cl ': clas)

analyseCD :: ClockedData ('Clocked a cl ': cls) -> Either a (ClockedData cls)
analyseCD (CDLeft a) = Left a
analyseCD (CDRight cd) = Right cd

-- type family ClockedData (clocked :: [Clocked]) :: [Type] where
--   ClockedData '[] = '[]
--   ClockedData ('Clocked a _cl ': clocked) = a ': ClockedData clocked

data HeterogeneousSum (as :: [Type]) where
  HLeft :: a -> HeterogeneousSum (a ': as)
  HRight :: HeterogeneousSum as -> HeterogeneousSum (a ': as)

clockErasure ::
  Monad m =>
  td ->
  SN td inClocks internalClocks outClocks m ->
  MSF m
    (td, Tag (Clocks td internalClocks), ClockedData inClocks)
    (ClockedData outClocks)
clockErasure initialTime (Synchronous clsf sn) = case internalClocksMatchTimeDomain sn of
  Refl -> proc (time, tag, input) -> do
    case (tag, analyseCD input) of
      (Left tagClSF, Left a) -> do
        b <- eraseClockClSF LeafProxy initialTime clsf -< (time, tagClSF, a)
        returnA -< CDLeft b
      (Right tagClocks, Right a) -> do
        output <- clockErasure initialTime sn -< (time, tagClocks, a)
        returnA -< CDRight output
      _ -> error "clockErasure: Impossible pattern in input (Left, Right)/(Right, Left)" -< ()
clockErasure initialTime (Resampling rb sn) = proc (time, tag, input) -> do
  case (tag, analyseCD input) of
    (Left tagL, Left a) -> do
      b <- eraseClockResBuf LeafProxy LeafProxy initialTime rb -< _
      returnA -< _

internalClocksMatchTimeDomain :: SN td inClocks internalClocks outClocks m -> td :~: Time (Clocks td internalClocks)
internalClocksMatchTimeDomain Empty = error "Hah. Yes. Well... turns out empty list is not a clock, so this is sort of moot"
internalClocksMatchTimeDomain (Synchronous _ sn) = case internalClocksMatchTimeDomain sn of
  Refl -> Refl
internalClocksMatchTimeDomain (Resampling _ sn) = internalClocksMatchTimeDomain sn
internalClocksMatchTimeDomain (PermuteInternalClocks sn Identity) = internalClocksMatchTimeDomain sn
