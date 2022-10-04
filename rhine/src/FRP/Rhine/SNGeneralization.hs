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
module FRP.Rhine.SNGeneralization where
import Data.Kind (Type)
import FRP.Rhine.ClSF
import FRP.Rhine.ResamplingBuffer
import Data.HList
import FRP.Rhine.Reactimation.ClockErasure (eraseClockClSF, eraseClockResBuf)
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Schedule (In, Out)
import Data.Void

data Plug = Closed | Open Type
data Clocked = Clocked Plug Type Plug

data SN td (clocks :: [Clocked]) (m :: Type -> Type) where
  Empty :: SN td '[] m
  Synchronous ::
    (Clock m cl, td ~ Time cl, cl ~ In cl, cl ~ Out cl) =>
    ClSF m cl a b ->
    SN td clocks m ->
    SN td ('Clocked (Open a) cl (Open b) ': clocks) m
  ResamplingHere ::
    (td ~ Time cl1, td ~ Time cl2) =>
    ResBuf m cl1 cl2 a b ->
    SN td ('Clocked (Open b) cl2 c ': 'Clocked d cl1 (Open a) ': clocks) m ->
    SN td ('Clocked Closed cl2 c ': 'Clocked d cl1 Closed ': clocks) m
  DoneIn ::
    SN td ('Clocked (Open a) cl b ': clocks) m ->
    SN td ('Clocked Closed cl b ': clocks) m
  -- TODO Or do we want to be able to erase arbitrary output?
  DoneOut ::
    SN td ('Clocked a cl (Open b) ': clocks) m ->
    SN td ('Clocked a cl Closed ': clocks) m
  ConcatInNil ::
    SN td clocks m ->
    SN td ('Clocked (Open (HList '[])) (Clocks td '[]) (Open (HList '[])) ': clocks) m
  ConcatInConsOO ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HList bs)) ': 'Clocked (Open a) cl (Open b) ': clocks) m ->
    SN td ('Clocked (Open (HList (a ': as))) (Clocks td (cl ': cls)) (Open (HList (b ': bs))) ': clocks) m
  ConcatInConsOC ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HList bs)) ': 'Clocked (Open a) cl Closed ': clocks) m ->
    SN td ('Clocked (Open (HList (a ': as))) (Clocks td (cl ': cls)) (Open (HList bs)) ': clocks) m
  ConcatInConsCO ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HList bs)) ': 'Clocked Closed cl (Open b) ': clocks) m ->
    SN td ('Clocked (Open (HList as)) (Clocks td (cl ': cls)) (Open (HList (b ': bs))) ': clocks) m
  ConcatInConsCC ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HList bs)) ': 'Clocked Closed cl Closed ': clocks) m ->
    SN td ('Clocked (Open (HList as)) (Clocks td (cl ': cls)) (Open (HList bs)) ': clocks) m

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

data InClocked (inClocks :: [Clocked]) where
  InHere :: a -> InClocked ('Clocked (Open a) cl b ': cls)
  InThere :: InClocked cls -> InClocked ('Clocked a cl b ': cls)

analyseIn :: InClocked ('Clocked (Open a) cl b ': cls) -> Either e (InClocked cls)
analyseIn (InHere a) = Left a
analyseIn (InThere cls) = Right $ analyseIn cls

data OutClocked (outClocks :: [Clocked]) where
  OutHere :: b -> OutClocked ('Clocked a cl (Open b) ': cls)
  OutThere :: OutClocked cls -> OutClocked ('Clocked a cl b ': cls)

data TagClocked (clocks :: [Clocked]) where
  TagHere :: Tag cl -> TagClocked ('Clocked a cl b ': cls)
  TagThere :: TagClocked cls -> TagClocked ('Clocked a cl b ': cls)

analyseTag :: TagClocked ('Clocked a cl b ': cls) -> Either (Tag cl) (TagClocked cls)
analyseTag (TagHere tag) = Left tag
analyseTag (TagThere cls) = Right $ analyseTag cls

-- data ClockedData (clocked :: [Clocked]) where
--   CDLeft :: a -> ClockedData ('Clocked a cl ': clas)
--   CDRight :: ClockedData clas -> ClockedData ('Clocked a cl ': clas)

-- analyseCD :: ClockedData ('Clocked a cl ': cls) -> Either a (ClockedData cls)
-- analyseCD (CDLeft a) = Left a
-- analyseCD (CDRight cd) = Right cd

-- type family ClockedData (clocked :: [Clocked]) :: [Type] where
--   ClockedData '[] = '[]
--   ClockedData ('Clocked a _cl ': clocked) = a ': ClockedData clocked

data HeterogeneousSum (as :: [Type]) where
  HLeft :: a -> HeterogeneousSum (a ': as)
  HRight :: HeterogeneousSum as -> HeterogeneousSum (a ': as)

clockErasure ::
  Monad m =>
  td ->
  SN td clocks m ->
  MSF m
    (td, TagClocked clocks, InClocked clocks)
    (InClocked clocks)
clockErasure initialTime (Synchronous clsf sn) = case internalClocksMatchTimeDomain sn of
  Refl -> proc (time, tag, input) -> do
    case (analyseTag tag, analyseIn input) of
      (TagHere tagClSF, InHere a) -> do
        b <- eraseClockClSF LeafProxy initialTime clsf -< (time, tagClSF, a)
        returnA -< OutHere b
      (Right tagClocks, Right a) -> do
        output <- clockErasure initialTime sn -< (time, tagClocks, a)
        returnA -< CDRight output
      _ -> error "clockErasure: Impossible pattern in input (Left, Right)/(Right, Left)" -< ()
clockErasure initialTime (ResamplingHere rb sn) = proc (time, tag, input) -> do
  case (tag, analyseCD input) of
    (Left tagL, Left a) -> do
      b <- eraseClockResBuf LeafProxy LeafProxy initialTime rb -< _
      returnA -< _
