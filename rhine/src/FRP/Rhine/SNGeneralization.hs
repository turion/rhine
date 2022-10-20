{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE LambdaCase #-}
module FRP.Rhine.SNGeneralization
  ( SN (..)
  , Clocked (..)
  , HeterogeneousSum (..)
  , Clocks (..)
  , clockErasure
  , concatSN
  ) where
import Data.Kind (Type)
import FRP.Rhine.ClSF
import FRP.Rhine.ResamplingBuffer
import Data.HList
import FRP.Rhine.Reactimation.ClockErasure (eraseClockClSF)
import Data.Void
import FRP.Rhine.Clock.Util (genTimeInfo')
import Data.Bifunctor

data Plug = Closed | Open Type
data Clocked = Clocked Plug Type Plug

data ClSFs td (clocks :: [Clocked]) (m :: Type -> Type) where
  Empty :: ClSFs td '[] m
  Synchronous ::
    (Clock m cl, td ~ Time cl) =>
    ClSF m cl a b ->
    ClSFs td clocks m ->
    ClSFs td ('Clocked (Open a) cl (Open b) ': clocks) m

data SN td (clocks :: [Clocked]) (m :: Type -> Type) where
  ClSFs ::
    ClSFs td clocks m ->
    SN td clocks m
  Resampling ::
    (td ~ Time cl1, td ~ Time cl2) =>
    ResBuf m cl1 cl2 a b ->
    ClosingIn clocksBefore clocksIntermediate b cl2 ->
    ClosingOut clocksIntermediate clocksAfter a cl1 ->
    SN td clocksBefore m ->
    SN td clocksAfter m
  DoneIn ::
    ClosingIn clocksBefore clocksAfter () cl ->
    SN td clocksBefore m ->
    SN td clocksAfter m
  DoneOut ::
    ClosingOut clocksBefore clocksAfter () cl ->
    SN td clocksBefore m ->
    SN td clocksAfter m
  ConcatInNil ::
    SN td clocks m ->
    SN td ('Clocked (Open (HList '[])) (Clocks td '[]) (Open (HeterogeneousSum '[])) ': clocks) m
  ConcatInConsOO ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HeterogeneousSum bs)) ': 'Clocked (Open a) cl (Open b) ': clocks) m ->
    SN td ('Clocked (Open (HList (a ': as))) (Clocks td (cl ': cls)) (Open (HeterogeneousSum (b ': bs))) ': clocks) m
  ConcatInConsOC ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HeterogeneousSum bs)) ': 'Clocked (Open a) cl Closed ': clocks) m ->
    SN td ('Clocked (Open (HList (a ': as))) (Clocks td (cl ': cls)) (Open (HeterogeneousSum (() ': bs))) ': clocks) m
  ConcatInConsCO ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HeterogeneousSum bs)) ': 'Clocked Closed cl (Open b) ': clocks) m ->
    SN td ('Clocked (Open (HList as)) (Clocks td (cl ': cls)) (Open (HeterogeneousSum (b ': bs))) ': clocks) m
  ConcatInConsCC ::
    SN td ('Clocked (Open (HList as)) (Clocks td cls) (Open (HeterogeneousSum bs)) ': 'Clocked Closed cl Closed ': clocks) m ->
    SN td ('Clocked (Open (HList as)) (Clocks td (cl ': cls)) (Open (HeterogeneousSum bs)) ': clocks) m

newtype Clocks (td :: Type) (cls :: [Type]) = Clocks (HList cls)

instance TimeDomain td => Clock m (Clocks td '[]) where
  type Time (Clocks td '[]) = td
  type Tag (Clocks td '[]) = Void
  initClock _ = error "Empty clock doesn't tick"

instance (TimeDomain td, Clock m cl, Clock m (Clocks td cls), Time cl ~ Time (Clocks td cls)) => Clock m (Clocks td (cl ': cls)) where
  type Time (Clocks td (cl ': cls)) = td
  type Tag (Clocks td (cl ': cls)) = Either (Tag cl) (Tag (Clocks td cls))
  initClock cl = error "Not yet implemented"

data Permutation (before :: [a]) (after :: [a]) where
  Identity :: Permutation as as
  SwapAnd :: Swap before intermediate -> Permutation intermediate after -> Permutation before after

deriving instance (Show (Permutation before after))

data Swap (before :: [a]) (after :: [a]) where
  Here :: Swap (a1 ': a2 ': as) (a2 ': a1 ': as)
  There :: Swap before after -> Swap (a ': before) (a ': after)

deriving instance (Show (Swap before after))

data ClosingIn (clocksBefore :: [Clocked]) (clocksAfter :: [Clocked]) a cl where
  ClosingInHere :: ClosingIn ('Clocked (Open a) cl b ': cls) ('Clocked Closed cl b ': cls) a cl
  ClosingInThere :: ClosingIn clocksBefore clocksAfter a cl -> ClosingIn (clocked ': clocksBefore) (clocked ': clocksAfter) a cl

data ClosingOut (clocksBefore :: [Clocked]) (clocksAfter :: [Clocked]) b cl where
  ClosingOutHere :: ClosingOut ('Clocked a cl (Open b) ': cls) ('Clocked a cl Closed ': cls) b cl
  ClosingOutThere :: ClosingOut clocksBefore clocksAfter b cl -> ClosingOut (clocked ': clocksBefore) (clocked ': clocksAfter) b cl

-- FIXME I should combine input and tag?
data InClocked (inClocks :: [Clocked]) where
  InHere :: a -> InClocked ('Clocked (Open a) cl b ': cls)
  InNoop :: InClocked ('Clocked Closed cl b ': cls)
  InThere :: InClocked cls -> InClocked ('Clocked a cl b ': cls)

analyseIn :: InClocked ('Clocked (Open a) cl b ': cls) -> Either a (InClocked cls)
analyseIn (InHere a) = Left a
analyseIn (InThere cls) = Right cls

data OutClocked (outClocks :: [Clocked]) where
  OutHere :: b -> OutClocked ('Clocked a cl (Open b) ': cls)
  OutNoop :: OutClocked ('Clocked a cl Closed ': cls)
  OutThere :: OutClocked cls -> OutClocked ('Clocked a cl b ': cls)

data TagClocked (clocks :: [Clocked]) where
  TagHere :: Tag cl -> TagClocked ('Clocked a cl b ': cls)
  TagThere :: TagClocked cls -> TagClocked ('Clocked a cl b ': cls)

analyseTag :: TagClocked ('Clocked a cl b ': cls) -> Either (Tag cl) (TagClocked cls)
analyseTag (TagHere tag) = Left tag
analyseTag (TagThere cls) = Right cls

data HeterogeneousSum (as :: [Type]) where
  HLeft :: a -> HeterogeneousSum (a ': as)
  HRight :: HeterogeneousSum as -> HeterogeneousSum (a ': as)

clockErasure' ::
  (Monad m, TimeDomain td) =>
  td ->
  ClSFs td clocks m ->
  MSF m
    (td, TagClocked clocks, InClocked clocks)
    (OutClocked clocks)

clockErasure' initialTime (Synchronous clsf clsfs) = proc (time, tag, input) -> do
  case (analyseTag tag, analyseIn input) of
    (Left tagClSF, Left a) -> do
      b <- eraseClockClSF initialTime clsf -< (time, tagClSF, a)
      returnA -< OutHere b
    (Right tagClocks, Right a) -> do
      output <- clockErasure' initialTime clsfs -< (time, tagClocks, a)
      returnA -< OutThere output
    _ -> error "clockErasure': Impossible pattern in input (Left, Right)/(Right, Left)" -< ()

clockErasure' _initialTime Empty = proc (_, tag, _) -> do
  -- See https://gitlab.haskell.org/ghc/ghc/-/issues/22294
  returnA -< case tag of
    -- Empty input clocks => no input constructible

clockErasure ::
  (Monad m, TimeDomain td) =>
  td ->
  SN td clocks m ->
  MSF m
    (td, TagClocked clocks, InClocked clocks)
    (OutClocked clocks)

clockErasure initialTime (ClSFs clsfs) = clockErasure' initialTime clsfs

clockErasure initialTime (Resampling rb0 closingIn closingOut sn) = feedback rb0 $ proc ((time, tag, input), rb) -> do
  let tagBefore = sendTag tag closingIn closingOut
  (inSN, rb') <- case (sendIn input closingIn closingOut, sendTag1 tag closingIn closingOut) of
    (Left mkInput, Left tagL) -> do
      timeInfo <- genTimeInfo' initialTime -< (time, tagL)
      (b, rb') <- arrM $ uncurry get -< (rb, timeInfo)
      returnA -< (mkInput b, rb')
    (Right inSN, Right _) -> do
      returnA -< (inSN, rb)
    _ -> error "ui" -< ()
  output <- clockErasure initialTime sn -< (time, tagBefore, inSN)
  case (sendOut output closingIn closingOut, sendTag2 tag closingIn closingOut) of
    ((Just a, output'), Left tagL) -> do
      timeInfo <- genTimeInfo' initialTime -< (time, tagL)
      rb'' <- arrM $ uncurry $ uncurry put -< ((rb', timeInfo), a)
      returnA -< (output', rb'')
    ((Nothing, output'), Right _) -> do
      returnA -< (output', rb')
    _ -> error "heieiei" -< ()

clockErasure initialTime (DoneIn closingIn sn) = proc (time, tag, input) -> do
  output <- clockErasure initialTime sn -< (time, closingInDoesntMatterOnTagclocked tag closingIn, either ($ ()) id $ sendIn' input closingIn)
  returnA -< closingInDoesntMatterOnOutclocked output closingIn

clockErasure initialTime (DoneOut closingOut sn) = proc (time, tag, input) -> do
  output <- clockErasure initialTime sn -< (time, closingOutDoesntMatterOnTagclocked tag closingOut, closingOutDoesntMatterOnInclocked input closingOut)
  returnA -< snd $ sendOut' output closingOut

clockErasure initialTime (ConcatInNil sn) = proc inputs -> do
  case inputs of
    (time, TagThere tag, InThere input) -> do
      output <- clockErasure initialTime sn -< (time, tag, input)
      returnA -< OutThere output
    _ -> error "clockErasure initialTime (ConcatInNil sn): Impossible input" -< ()

clockErasure initialTime (ConcatInConsOO sn) = proc (time, tag, input) -> do
  let
    (tag', input') = case (tag, input) of
      (TagHere (Left tagHereL), InHere (HCons a _)) -> (TagThere $ TagHere tagHereL, InThere $ InHere a)
      (TagHere (Right tagHereR), InHere (HCons _ as)) -> (TagHere tagHereR, InHere as)
      (TagThere tagThere, InThere inThere) -> (TagThere $ TagThere tagThere, InThere $ InThere inThere)
      _ -> error "clockErasure _ (ConcatInConsOO sn): Impossible input"
  output <- clockErasure initialTime sn -< (time, tag', input')
  returnA -< case output of
    OutHere bs -> OutHere $ HRight bs
    OutThere (OutHere b) -> OutHere $ HLeft b
    OutThere (OutThere outThere) -> OutThere outThere

clockErasure initialTime (ConcatInConsOC sn) = proc (time, tag, input) -> do
  let
    (tag', input') = case (tag, input) of
      (TagHere (Left tagHereL), InHere (HCons a _)) -> (TagThere $ TagHere tagHereL, InThere $ InHere a)
      (TagHere (Right tagHereR), InHere (HCons _ as)) -> (TagHere tagHereR, InHere as)
      (TagThere tagThere, InThere inThere) -> (TagThere $ TagThere tagThere, InThere $ InThere inThere)
      _ -> error "clockErasure _ (ConcatInConsOC sn): Impossible input"
  output <- clockErasure initialTime sn -< (time, tag', input')
  returnA -< case output of
    OutHere bs -> OutHere $ HRight bs
    OutThere OutNoop -> OutHere $ HLeft ()
    OutThere (OutThere outThere) -> OutThere outThere
clockErasure initialTime (ConcatInConsCO sn) = proc (time, tag, input) -> do
  let
    (tag', input') = case (tag, input) of
      (TagHere (Right tagHereR), InHere as) -> (TagHere tagHereR, InHere as)
      (TagThere tagThere, InThere inThere) -> (TagThere $ TagThere tagThere, InThere $ InThere inThere)
      _ -> error "clockErasure _ (ConcatInConsOO sn): Impossible input"
  output <- clockErasure initialTime sn -< (time, tag', input')
  returnA -< case output of
    OutHere bs -> OutHere $ HRight bs
    OutThere (OutHere b) -> OutHere $ HLeft b
    OutThere (OutThere outThere) -> OutThere outThere
clockErasure initialTime (ConcatInConsCC _) = _

sendIn ::
  InClocked clocksAfter ->
  ClosingIn clocksBefore clocksIntermediate b cl2 ->
  ClosingOut clocksIntermediate clocksAfter a cl1 ->
  Either (b -> InClocked clocksBefore) (InClocked clocksBefore)
sendIn inClocked closingIn closingOut = sendIn' (closingOutDoesntMatterOnInclocked inClocked closingOut) closingIn

sendIn' ::
  InClocked clocksIntermediate ->
  ClosingIn clocksBefore clocksIntermediate b cl2 ->
  Either (b -> InClocked clocksBefore) (InClocked clocksBefore)
sendIn' InNoop ClosingInHere = Left InHere
sendIn' InNoop (ClosingInThere _) = Right InNoop
sendIn' (InHere a) (ClosingInThere _) = Right (InHere a)
sendIn' (InThere inClocked) ClosingInHere = Right (InThere inClocked)
sendIn' (InThere inClocked) (ClosingInThere pointer) = bimap (fmap InThere) InThere $ sendIn' inClocked pointer

closingOutDoesntMatterOnInclocked ::
  InClocked clocksAfter ->
  ClosingOut clocksIntermediate clocksAfter a cl ->
  InClocked clocksIntermediate
closingOutDoesntMatterOnInclocked (InHere a) ClosingOutHere = InHere a
closingOutDoesntMatterOnInclocked (InHere a) (ClosingOutThere _) = InHere a
closingOutDoesntMatterOnInclocked InNoop ClosingOutHere = InNoop
closingOutDoesntMatterOnInclocked InNoop (ClosingOutThere _) = InNoop
closingOutDoesntMatterOnInclocked (InThere inClocked) ClosingOutHere = InThere inClocked
closingOutDoesntMatterOnInclocked (InThere inClocked) (ClosingOutThere pointer) = InThere $ closingOutDoesntMatterOnInclocked inClocked pointer

sendOut ::
  OutClocked clocksBefore ->
  ClosingIn clocksBefore clocksIntermediate a cl1 ->
  ClosingOut clocksIntermediate clocksAfter b cl2 ->
  (Maybe b, OutClocked clocksAfter)
sendOut inClocked closingIn closingOut = sendOut' (closingInDoesntMatterOnOutclocked inClocked closingIn) closingOut

sendOut' ::
  OutClocked clocksIntermediate ->
  ClosingOut clocksIntermediate clocksAfter b cl2 ->
  (Maybe b, OutClocked clocksAfter)
sendOut' (OutHere b) ClosingOutHere = (Just b, OutNoop)
sendOut' OutNoop (ClosingOutThere _) = (Nothing, OutNoop)
sendOut' (OutHere a) (ClosingOutThere _) = (Nothing, OutHere a)
sendOut' (OutThere outClocked) ClosingOutHere = (Nothing, OutThere outClocked)
sendOut' (OutThere outClocked) (ClosingOutThere closingOutThere) = OutThere <$> sendOut' outClocked closingOutThere

closingInDoesntMatterOnOutclocked ::
  OutClocked clocksBefore ->
  ClosingIn clocksBefore clocksIntermediate a cl ->
  OutClocked clocksIntermediate
closingInDoesntMatterOnOutclocked (OutHere a) ClosingInHere = OutHere a
closingInDoesntMatterOnOutclocked (OutHere a) (ClosingInThere _) = OutHere a
closingInDoesntMatterOnOutclocked OutNoop ClosingInHere = OutNoop
closingInDoesntMatterOnOutclocked OutNoop (ClosingInThere _) = OutNoop
closingInDoesntMatterOnOutclocked (OutThere inClocked) ClosingInHere = OutThere inClocked
closingInDoesntMatterOnOutclocked (OutThere inClocked) (ClosingInThere closingOutThere) = OutThere $ closingInDoesntMatterOnOutclocked inClocked closingOutThere

sendTag ::
  TagClocked clocksAfter ->
  ClosingIn clocksBefore clocksIntermediate a cl1 ->
  ClosingOut clocksIntermediate clocksAfter b cl2 ->
  TagClocked clocksBefore
sendTag tagClocked closingIn closingOut = closingInDoesntMatterOnTagclocked (closingOutDoesntMatterOnTagclocked tagClocked closingOut) closingIn

sendTag1 ::
  TagClocked clocksAfter ->
  ClosingIn clocksBefore clocksIntermediate a cl1 ->
  ClosingOut clocksIntermediate clocksAfter b cl2 ->
  Either (Tag cl1) (TagClocked clocksBefore)
sendTag1 tagClocked closingIn closingOut = sendTag1' (closingOutDoesntMatterOnTagclocked tagClocked closingOut) closingIn

sendTag1' ::
  TagClocked clocksIntermediate ->
  ClosingIn clocksBefore clocksIntermediate a cl1 ->
  Either (Tag cl1) (TagClocked clocksBefore)
sendTag1' (TagHere tag) ClosingInHere = Left tag
sendTag1' (TagHere tag) (ClosingInThere _) = Right (TagHere tag)
sendTag1' (TagThere tagThere) ClosingInHere = Right (TagThere tagThere)
sendTag1' (TagThere tagThere) (ClosingInThere closingInThere) = TagThere <$> sendTag1' tagThere closingInThere

sendTag2 ::
  TagClocked clocksAfter ->
  ClosingIn clocksBefore clocksIntermediate a cl1 ->
  ClosingOut clocksIntermediate clocksAfter b cl2 ->
  Either (Tag cl2) (TagClocked clocksBefore)
sendTag2 tagClocked closingIn closingOut = flip closingInDoesntMatterOnTagclocked closingIn <$> sendTag2' tagClocked closingOut

sendTag2' ::
  TagClocked clocksAfter ->
  ClosingOut clocksIntermediate clocksAfter b cl2 ->
  Either (Tag cl2) (TagClocked clocksIntermediate)
sendTag2' (TagHere tag) ClosingOutHere = Left tag
sendTag2' (TagHere tag) (ClosingOutThere _) = Right (TagHere tag)
sendTag2' (TagThere tagThere) ClosingOutHere = Right (TagThere tagThere)
sendTag2' (TagThere tagThere) (ClosingOutThere closingInThere) = TagThere <$> sendTag2' tagThere closingInThere

closingOutDoesntMatterOnTagclocked ::
  TagClocked clocksAfter ->
  ClosingOut clocksIntermediate clocksAfter a cl ->
  TagClocked clocksIntermediate
closingOutDoesntMatterOnTagclocked (TagHere tag) ClosingOutHere = TagHere tag
closingOutDoesntMatterOnTagclocked (TagHere tag) (ClosingOutThere _) = TagHere tag
closingOutDoesntMatterOnTagclocked (TagThere tagClocked) ClosingOutHere = TagThere tagClocked
closingOutDoesntMatterOnTagclocked (TagThere tagClocked) (ClosingOutThere closingOutThere) = TagThere $ closingOutDoesntMatterOnTagclocked tagClocked closingOutThere

closingInDoesntMatterOnTagclocked ::
  TagClocked clocksIntermediate ->
  ClosingIn clocksBefore clocksIntermediate a cl ->
  TagClocked clocksBefore
closingInDoesntMatterOnTagclocked (TagHere tag) ClosingInHere = TagHere tag
closingInDoesntMatterOnTagclocked (TagHere tag) (ClosingInThere _) = TagHere tag
closingInDoesntMatterOnTagclocked (TagThere tagClocked) ClosingInHere = TagThere tagClocked
closingInDoesntMatterOnTagclocked (TagThere tagClocked) (ClosingInThere closingInThere) = TagThere $ closingInDoesntMatterOnTagclocked tagClocked closingInThere

type family Concat (clocks1 :: [Clocked]) (clocks2 :: [Clocked]) :: [Clocked] where
  Concat '[] clocks = clocks
  Concat (clocked ': clocks1) clocks2 = clocked ': Concat clocks1 clocks2

concatClSFs :: ClSFs td clocks1 m -> ClSFs td clocks2 m -> ClSFs td (Concat clocks1 clocks2) m
concatClSFs Empty sn2 = sn2
concatClSFs (Synchronous clsf sn1) sn2 = Synchronous clsf $ concatClSFs sn1 sn2

pushClosingInPastClSFs ::
  ClosingIn clocksBefore clocksIntermediate a cl ->
  ClSFs td clocks m ->
  ClosingIn (Concat clocks clocksBefore) (Concat clocks clocksIntermediate) a cl
pushClosingInPastClSFs closingIn (Synchronous _clsf clsfs) = ClosingInThere $ pushClosingInPastClSFs closingIn clsfs
pushClosingInPastClSFs closingIn Empty = closingIn

pushClosingOutPastClSFs ::
  ClosingOut clocksIntermediate clocksAfter a cl ->
  ClSFs td clocks m ->
  ClosingOut (Concat clocks clocksIntermediate) (Concat clocks clocksAfter) a cl
pushClosingOutPastClSFs closingIn (Synchronous _clsf clsfs) = ClosingOutThere $ pushClosingOutPastClSFs closingIn clsfs
pushClosingOutPastClSFs closingIn Empty = closingIn

concatSN :: SN td clocks1 m -> SN td clocks2 m -> SN td (Concat clocks1 clocks2) m
concatSN (ClSFs clsfs) (Resampling rb closingIn closingOut sn1) = Resampling rb (pushClosingInPastClSFs closingIn clsfs) (pushClosingOutPastClSFs closingOut clsfs)
  $ concatSN (ClSFs clsfs) sn1
concatSN (ClSFs clsfs1) (ClSFs clsfs2) = ClSFs $ concatClSFs clsfs1 clsfs2
concatSN (ClSFs clsfs) (DoneIn closingIn sn) = DoneIn (pushClosingInPastClSFs closingIn clsfs) $ concatSN clsfs sn
concatSN (Resampling rb closingIn closingOut sn1) sn2 = Resampling rb (concatClosingIn closingIn sn2) (concatClosingOut closingOut sn2) $ concatSN sn1 sn2
concatSN (DoneIn closingIn sn1) sn2 = DoneIn (concatClosingIn closingIn sn2) $ concatSN sn1 sn2
concatSN (DoneOut closingOut sn1) sn2 = DoneOut (concatClosingOut closingOut sn2) $ concatSN sn1 sn2
concatSN (ConcatInNil sn1) sn2 = ConcatInNil $ concatSN sn1 sn2
concatSN (ConcatInConsOO sn1) sn2 = ConcatInConsOO $ concatSN sn1 sn2
concatSN (ConcatInConsOC sn1) sn2 = ConcatInConsOC $ concatSN sn1 sn2
concatSN (ConcatInConsCO sn1) sn2 = ConcatInConsCO $ concatSN sn1 sn2
concatSN (ConcatInConsCC sn1) sn2 = ConcatInConsCC $ concatSN sn1 sn2

concatClosingIn ::
  ClosingIn clocksBefore clocksIntermediate a cl ->
  SN td clocks2 m ->
  ClosingIn (Concat clocksBefore clocks2) (Concat clocksIntermediate clocks2) a cl
concatClosingIn ClosingInHere _ = ClosingInHere
concatClosingIn (ClosingInThere closingInThere) sn = ClosingInThere $ concatClosingIn closingInThere sn

concatClosingOut ::
  ClosingOut clocksBefore clocksOuttermediate a cl ->
  SN td clocks2 m ->
  ClosingOut (Concat clocksBefore clocks2) (Concat clocksOuttermediate clocks2) a cl
concatClosingOut ClosingOutHere _ = ClosingOutHere
concatClosingOut (ClosingOutThere closingOutThere) sn = ClosingOutThere $ concatClosingOut closingOutThere sn

class InputMatchesOutput clocksIn clocksOut where
  oi :: OutClocked clocksOut -> InClocked clocksIn

instance InputMatchesOutput '[] '[] where
  oi = \case

instance InputMatchesOutput clocksIn clocksOut => InputMatchesOutput ('Clocked b cl c ': clocksIn) ('Clocked a cl b ': clocksOut) where

-- FIXME this would be easier if I had one type that only collects ClSFs, then one that only attaches ResBufs, then one that does the remaining operations
permuteSN ::
  Permutation before after ->
  SN td before m ->
  SN td after m
permuteSN Identity sn = sn
permuteSN (SwapAnd Here permutation) (Synchronous clsf1 (Synchronous clsf2 sn)) = permuteSN permutation $ Synchronous clsf2 $ Synchronous clsf1 sn
permuteSN (SwapAnd Here permutation) (Synchronous clsf (Resampling rb closingIn closingOut sn)) = _
permuteSN (SwapAnd (There swap) permutation) (Synchronous clsf1 sn) = permuteSN permutation $ Synchronous clsf1 $ permuteSN (SwapAnd swap Identity) sn
permuteSN (SwapAnd Here Identity) (Resampling rb ClosingInHere ClosingOutHere (Synchronous clsf1 (Synchronous clsf2 Empty))) =
  Resampling rb (ClosingInThere ClosingInHere) (ClosingOutThere ClosingOutHere) $
  Synchronous clsf2 $
  Synchronous clsf1 Empty
permuteSN permutation (Resampling rb closingIn closingOut sn) = _
permuteSN permutation (DoneIn _ sn) = _
permuteSN permutation (DoneOut _ sn) = _
permuteSN permutation (ConcatInNil sn) = _
permuteSN permutation (ConcatInConsCC sn) = _
permuteSN permutation (ConcatInConsCO sn) = _
permuteSN permutation (ConcatInConsOC sn) = _
permuteSN permutation (ConcatInConsOO sn) = _

skipClosingIn ::
  ClosingIn (clockBefore ': clocksBefore) (clockIntermediate ': clocksIntermediate) a cl ->
  ClosingIn (clockBefore ': clBetween ': clocksBefore) (clockIntermediate ': clBetween ': clocksIntermediate)  a cl
skipClosingIn ClosingInHere = ClosingInHere
skipClosingIn (ClosingInThere closingInThere) = ClosingInThere (ClosingInThere closingInThere)

skipClosingOut ::
  ClosingOut (clockBefore ': clocksBefore) (clockOuttermediate ': clocksOuttermediate) a cl ->
  ClosingOut (clockBefore ': clBetween ': clocksBefore) (clockOuttermediate ': clBetween ': clocksOuttermediate)  a cl
skipClosingOut ClosingOutHere = ClosingOutHere
skipClosingOut (ClosingOutThere closingOutThere) = ClosingOutThere (ClosingOutThere closingOutThere)
