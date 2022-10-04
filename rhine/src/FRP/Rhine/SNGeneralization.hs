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
module FRP.Rhine.SNGeneralization where
import Data.Kind (Type)
import FRP.Rhine.ClSF
import FRP.Rhine.ResamplingBuffer
import Data.HList

-- FIXME Clocked a cl or Clocked cl a?
data Clocked = Clocked Type Type

data SN (inClocks :: [Clocked]) (internalClocks :: [Type]) (outClocks :: [Clocked]) (m :: Type -> Type) where
  Synchronous ::
    ClSF m cl a b ->
    SN inClocks internalClocks outClocks m ->
    SN ('Clocked a cl ': inClocks) (cl ': internalClocks) ('Clocked b cl ': outClocks) m
  Resampling ::
    ResBuf m cl1 cl2 a b ->
    SN ('Clocked b cl2 ': inClocks) internalClocks ('Clocked a cl1 ': outClocks) m ->
    SN inClocks internalClocks outClocks m
  PermuteInClocks ::
    SN inClocksBefore internalClocks outClocks m ->
    Permutation inClocksBefore inClocksAfter ->
    SN inClocksAfter internalClocks outClocks m
  PermuteInternalClocks ::
    SN inClocks internalClocksBefore outClocks m ->
    Permutation internalClocksBefore internalClocksAfter ->
    SN inClocks internalClocksAfter outClocks m
  PermuteOutClocks ::
    SN inClocks internalClocks outClocksBefore m ->
    Permutation outClocksBefore outClocksAfter ->
    SN inClocks internalClocks outClocksAfter m
  DoneIn ::
    SN ('Clocked () cl ': inClocks) internalClocks outClocks m ->
    SN inClocks internalClocks outClocks m
  -- TODO Or do we want to be able to erase arbitrary output?
  DoneOut ::
    SN inClocks internalClocks ('Clocked () cl ': outClocks) m ->
    SN inClocks internalClocks outClocks m
  -- FIXME get rid of if nil & cons work
  ConcatIn2 ::
    SN ('Clocked a cl1 ': 'Clocked b cl2 ': inClocks) internalClocks outClocks m ->
    SN ('Clocked (HList '[a, b]) (Clocks '[cl1, cl2]) ': inClocks) internalClocks outClocks m
  -- TODO Replicate 3 of these. Not sure this can be refactored to reduce duplication
  ConcatInNil ::
    SN inClocks internalClocks outClocks m ->
    SN ('Clocked (HList '[]) (Clocks '[]) ': inClocks) internalClocks outClocks m
  ConcatInCons ::
    SN ('Clocked (HList as) (Clocks cls) ': 'Clocked a cl ': inClocks) internalClocks outClocks m ->
    SN ('Clocked (HList (a ': as)) (Clocks (cl ': cls)) ': inClocks) internalClocks outClocks m

data Clocks (cls :: [Type]) = Clocks (HList cls)

instance Clock m cl => Clock m (Clocks '[cl]) where
  type Time (Clocks '[cl]) = Time cl
  initClock cl = _

data Permutation (before :: [a]) (after :: [a]) where
  Identity :: Permutation as as
  SwapAnd :: Swap before intermediate -> Permutation intermediate after -> Permutation before after

deriving instance (Show (Permutation before after))

data Swap (before :: [a]) (after :: [a]) where
  Here :: Swap (a1 ': a2 ': as) (a2 ': a1 ': as)
  There :: Swap before after -> Swap (a ': before) (a ': after)

deriving instance (Show (Swap before after))

data InClocked (inClocks :: [Clocked]) where
  InNil :: InClocked '[]
  InCons :: a -> InClocked clas -> InClocked ('Clocked a cl ': clas)

data OutClocked (outClocks :: [Clocked]) where
  OutHere :: a -> OutClocked ('Clocked a cl ': clas)
  OutThere :: OutClocked clas -> OutClocked ('Clocked a cl ': clas)

clockErasure :: SN inClocks internalClocks outClocks m -> MSF m a b
clockErasure = _
