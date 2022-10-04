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
import Control.Monad.Trans.MSF (MSFExcept)

-- FIXME Clocked a cl or Clocked cl a?
data Clocked = Clocked Type Type

data SN (inClocks :: [Clocked]) (internalClocks :: [Type]) (outClocks :: [Clocked]) (m :: Type -> Type) e where
  SynchronousExcept :: ClSFExcept m cl a b e -> SN '[ 'Clocked a cl] '[cl] '[ 'Clocked b cl] m e
  Resampling ::
    ResBuf (ExceptT e m) cl1 cl2 a b ->
    SN ('Clocked b cl2 ': inClocks) internalClocks ('Clocked a cl1 ': outClocks) m e ->
    SN inClocks internalClocks outClocks m e
  -- TODO Alternatively, could make Synchronous a cons-like operator
  Combine ::
    SN inClocks1 internalClocks1 outClocks1 m e ->
    SN inClocks2 internalClocks2 outClocks2 m e ->
    SN (Concat inClocks1 inClocks2) (Concat internalClocks1 internalClocks2) (Concat outClocks1 outClocks2) m e
  Stop :: e -> SN inClocks internalClocks outClocks m e
  -- This is cool because it means I can rewire internally!
  Catch :: SN inClocks internalClocks outClocks m e1 -> (e1 -> SN inClocks internalClocks outClocks m e2) -> SN inClocks internalClocks outClocks m e2
  PermuteInClocks ::
    SN inClocksBefore internalClocks outClocks m e ->
    Permutation inClocksBefore inClocksAfter ->
    SN inClocksAfter internalClocks outClocks m e
  PermuteInternalClocks ::
    SN inClocks internalClocksBefore outClocks m e ->
    Permutation internalClocksBefore internalClocksAfter ->
    SN inClocks internalClocksAfter outClocks m e
  PermuteOutClocks ::
    SN inClocks internalClocks outClocksBefore m e ->
    Permutation outClocksBefore outClocksAfter ->
    SN inClocks internalClocks outClocksAfter m e
  DoneIn ::
    SN ('Clocked () cl ': inClocks) internalClocks outClocks m e ->
    SN inClocks internalClocks outClocks m e
  -- TODO Or do we want to be able to erase arbitrary output?
  DoneOut ::
    SN inClocks internalClocks ('Clocked () cl ': outClocks) m e ->
    SN inClocks internalClocks outClocks m e
  -- FIXME get rid of if nil & cons work
  ConcatIn2 ::
    SN ('Clocked a cl1 ': 'Clocked b cl2 ': inClocks) internalClocks outClocks m e ->
    SN ('Clocked (HList '[a, b]) (Clocks '[cl1, cl2]) ': inClocks) internalClocks outClocks m e
  -- TODO Replicate 3 of these. Not sure this can be refactored to reduce duplication
  ConcatInNil ::
    SN inClocks internalClocks outClocks m e ->
    SN ('Clocked (HList '[]) (Clocks '[]) ': inClocks) internalClocks outClocks m e
  ConcatInCons ::
    SN ('Clocked (HList as) (Clocks cls) ': 'Clocked a cl ': inClocks) internalClocks outClocks m e ->
    SN ('Clocked (HList (a ': as)) (Clocks (cl ': cls)) ': inClocks) internalClocks outClocks m e

data Clocks (cls :: [Type]) = Clocks (HList cls)

instance Clock m cl => Clock m (Clocks '[cl]) where

type family Concat (as :: [a]) (as' :: [a]) :: [a] where
  Concat '[] as' = as'
  Concat (a ': as) as' = a ': Concat as as'

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

clockErasure :: SN inClocks internalClocks outClocks m e -> MSF (ExceptT e m) a b
clockErasure = _
