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
import FRP.Rhine.Clock.Util (genTimeInfo, genTimeInfo')
import Data.Constraint (Dict (Dict), mapDict, withDict)
import Data.Bifunctor

data Plug = Closed | Open Type
data Clocked = Clocked Plug Type Plug

data SN td (clocks :: [Clocked]) (m :: Type -> Type) where
  Empty :: SN td '[] m
  Synchronous ::
    (Clock m cl, td ~ Time cl) =>
    ClSF m cl a b ->
    SN td clocks m ->
    SN td ('Clocked (Open a) cl (Open b) ': clocks) m
  ResamplingHere ::
    (td ~ Time cl1, td ~ Time cl2) =>
    ResBuf m cl1 cl2 a b ->
    SN td ('Clocked (Open b) cl2 c ': 'Clocked d cl1 (Open a) ': clocks) m ->
    SN td ('Clocked Closed cl2 c ': 'Clocked d cl1 Closed ': clocks) m
  ResamplingSomewhere ::
    (td ~ Time cl1, td ~ Time cl2) =>
    ResBuf m cl1 cl2 a b ->
    ClosingIn clocksBefore clocksIntermediate b cl2 ->
    ClosingOut clocksIntermediate clocksAfter a cl1 ->
    SN td clocksBefore m ->
    SN td clocksAfter m
  DoneIn ::
    SN td ('Clocked (Open a) cl b ': clocks) m ->
    SN td ('Clocked Closed cl b ': clocks) m
  -- TODO Or do we want to be able to erase arbitrary output?
  DoneOut ::
    SN td ('Clocked a cl (Open b) ': clocks) m ->
    SN td ('Clocked a cl Closed ': clocks) m
  ConcatInNil ::
    TimeDomain td =>
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
  initClock cl = error "Empty clock doesn't tick"

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

-- FIXME Rewrite with lenses
thereIn :: InClocked ('Clocked a cl b ': cls) -> Maybe (InClocked cls)
thereIn (InHere _) = Nothing
thereIn InNoop = Nothing
thereIn (InThere cls) = Just cls

data OutClocked (outClocks :: [Clocked]) where
  OutHere :: b -> OutClocked ('Clocked a cl (Open b) ': cls)
  OutNoop :: OutClocked ('Clocked a cl Closed ': cls)
  OutThere :: OutClocked cls -> OutClocked ('Clocked a cl b ': cls)

analyseOut :: OutClocked ('Clocked a cl (Open b) ': cls) -> Either b (OutClocked cls)
analyseOut (OutHere a) = Left a
analyseOut (OutThere cls) = Right cls

outNow :: OutClocked ('Clocked a cl b ': cls) -> OutClocked ('Clocked a' cl b ': cls')
outNow (OutHere b) = OutHere b
outNow OutNoop = OutNoop
outNow (OutThere _) = error "outNow: OutThere"

thereOut :: OutClocked ('Clocked a cl b ': cls) -> Maybe (OutClocked cls)
thereOut (OutHere _) = Nothing
thereOut OutNoop = Nothing
thereOut (OutThere cls) = Just cls

disregardIn :: OutClocked ('Clocked a cl b ': cls) -> OutClocked ('Clocked a' cl b ': cls)
disregardIn (OutHere b) = OutHere b
disregardIn OutNoop = OutNoop
disregardIn (OutThere cls) = OutThere cls

data TagClocked (clocks :: [Clocked]) where
  TagHere :: Tag cl -> TagClocked ('Clocked a cl b ': cls)
  TagThere :: TagClocked cls -> TagClocked ('Clocked a cl b ': cls)

analyseTag :: TagClocked ('Clocked a cl b ': cls) -> Either (Tag cl) (TagClocked cls)
analyseTag (TagHere tag) = Left tag
analyseTag (TagThere cls) = Right cls

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
  (Monad m, TimeDomain td) =>
  td ->
  SN td clocks m ->
  MSF m
    (td, TagClocked clocks, InClocked clocks)
    (OutClocked clocks)
clockErasure initialTime (Synchronous clsf sn) = proc (time, tag, input) -> do
  case (analyseTag tag, analyseIn input) of
    (Left tagClSF, Left a) -> do
      b <- eraseClockClSF initialTime clsf -< (time, tagClSF, a)
      returnA -< OutHere b
    (Right tagClocks, Right a) -> do
      output <- clockErasure initialTime sn -< (time, tagClocks, a)
      returnA -< OutThere output
    _ -> error "clockErasure: Impossible pattern in input (Left, Right)/(Right, Left)" -< ()
clockErasure initialTime snAll@(ResamplingHere rb0 sn) = feedback rb0 $ proc ((time, tag, input), rb) -> do
  (bMaybe, rb') <- case analyseTag tag of
    Left tagL -> do
      timeInfo <- withDict (isClocked snAll) $ genTimeInfo' initialTime -< (time, tagL)
      (b, rb') <- arrM $ uncurry get -< (rb, timeInfo)
      returnA -< (Just b, rb')
    Right _ -> do
      returnA -< (Nothing, rb)
  -- FIXME naming
  aMaybe <- case (analyseTag <$> analyseTag tag, thereIn <$> thereIn input) of
    (Right (Right tagRR), Just (Just inputRR)) -> do
      clockErasure initialTime sn -< (time, TagThere $ TagThere tagRR, InThere $ InThere inputRR)
    _ -> error "Oh noez" -< ()
  rb'' <- case (analyseTag <$> analyseTag tag, analyseOut <$> thereOut aMaybe) of
    (Right (Left tagRL), Just (Left a)) -> do
      timeInfo <- withDict (isClocked1 snAll) $ genTimeInfo' initialTime -< (time, tagRL)
      arrM $ uncurry $ uncurry put -< ((rb', timeInfo), a)
    _ -> do
      returnA -< rb'

  let
    output = case (analyseTag <$> analyseTag tag, thereOut <$> thereOut aMaybe) of
      (Left _, _) -> outNow aMaybe
      (Right (Left _), _) -> OutThere OutNoop
      (Right (Right _), Just (Just a)) -> OutThere $ OutThere a
      _ -> error "Wrong"
  returnA -< (output, rb'')
clockErasure initialTime (ResamplingSomewhere rb0 closingIn closingOut sn) = feedback rb0 $ proc ((time, tag, input), rb) -> do
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
clockErasure initialTime (ResamplingSomewhere rb0 ClosingInHere ClosingOutHere sn) = feedback rb0 $ proc ((time, tag, input), rb) -> do
  (inSN, tagSN, rb', timeInfoMaybe) <- case (analyseTag tag, thereIn input) of
    (Left tagHere, _) -> do
      timeInfo <- genTimeInfo' initialTime -< (time, tagHere)
      (b, rb') <- arrM $ uncurry get -< (rb, timeInfo)
      returnA -< (InHere b, TagHere tagHere, rb', Just timeInfo)
    (Right tagThere, Just inThere) -> do
      returnA -< (InThere inThere, TagThere tagThere, rb, Nothing)
    _ -> error "Can't be" -< ()
  c <- clockErasure initialTime sn -< (time, tagSN, inSN)
  case (c, timeInfoMaybe) of
    (OutHere a, Just timeInfo) -> do
      rb'' <- arrM $ uncurry $ uncurry put -< ((rb', timeInfo), a)
      returnA -< (OutNoop, rb'')
    (OutThere thing, Nothing) -> do
      returnA -< (OutThere thing, rb')
    _ -> do
      error "nay" -< ()
clockErasure initialTime (ResamplingSomewhere rb0 ClosingInHere (ClosingOutThere pointer) sn) = feedback rb0 $ proc ((time, tag, input), rb) -> do
  (inSN, tagSN, rb') <- case (analyseTag tag, thereIn input) of
    (Left tagHere, Nothing) -> do
      timeInfo <- genTimeInfo' initialTime -< (time, tagHere)
      (b, rb') <- arrM $ uncurry get -< (rb, timeInfo)
      returnA -< (InHere b, TagHere tagHere, rb')
    (Right tagThere, Just foo) -> do
      returnA -< (_, _, rb)
    _ -> error "noes" -< ()
  c <- clockErasure initialTime sn -< (time, tagSN, inSN)
  _ -< _
clockErasure _ _ = error "not yet implemented"

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

isClocked :: SN td ('Clocked a cl b ': clocks) m -> Dict (Clock m cl)
isClocked (Synchronous _ _) = Dict
isClocked (ResamplingHere _ sn) = isClocked sn
isClocked (DoneIn sn) = isClocked sn
isClocked (DoneOut sn) = isClocked sn
isClocked (ConcatInNil _) = Dict
isClocked (ConcatInConsOO sn) = mapDict _ Dict
isClocked _ = _

-- FIXME Abstract this?
isClocked1 :: SN td (clocked ': 'Clocked a cl b ': clocks) m -> Dict (Clock m cl)
isClocked1 (Synchronous _ sn) = isClocked sn
isClocked1 (ResamplingHere _ sn) = isClocked1 sn
isClocked1 (DoneIn sn) = isClocked1 sn
isClocked1 (DoneOut sn) = isClocked1 sn
isClocked1 (ConcatInNil sn) = isClocked sn
isClocked1 (ConcatInConsOO sn) = _ -- FIXME I need isClocked2 etc. for this
isClocked1 _ = _
