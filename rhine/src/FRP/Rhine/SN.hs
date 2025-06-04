{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}

module FRP.Rhine.SN (
  At (
    -- It is intentional _not_ to export the constructor Absent.
    -- It may only be used internally to guarantee clock safety,
    -- since otherwise a user could create an absent signal
    -- in a situation where the clock ticks.
    -- The constructor Present is harmless though, since an unneeded value is simply discarded.
    Present
  ),
  SNComponent (..),
  SN (..),
  Curried (..),
  eraseClockFreeSN,
  synchronous,
  resampling,
  feedbackSN,
  always,
  with,
  handle,
  -- now,
  currently,
  Clocks (..),
  NP (..),
  NS (..),
  (.:.),
  cnil,
  Append,
  Position, -- FIXME this should be internal
  HasClock (..),
  HasClocksOrdered (..),
  runClocks,
  -- FIXME the followong are probably internal
  appendClocks,
  appendClocksSN,
  prependClocksSN,
  ClassyClock (..),
  orderedPositionsInAppend,
  AtAny,
  AtSome (..),
  atAny,
  atThis,
  currentlyAny,
  Flip (..)
)
where

-- FIXME sort imports and exports

import Control.Arrow.Free
import Control.Category (Category)
import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Automaton.Trans.Except (performOnFirstSample)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Data.Stream.Result (Result (..))
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Kind (Type)
import Data.List.NonEmpty (fromList, toList)
import Data.Automaton (concatS)
import Data.Proxy (Proxy (..))
import Data.SOP (NP (..), NS (..), SListI, hmap, K (..))
import Data.Type.Equality ((:~:) (Refl))

import Data.Profunctor (Profunctor (..), WrappedArrow (..))

import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock (Clock (..), TimeInfo (..), tag)
import FRP.Rhine.Clock.Proxy (GetClockProxy (getClockProxy))
import FRP.Rhine.Clock.Util (genTimeInfo)
import FRP.Rhine.ResamplingBuffer (ResamplingBuffer (..))
import FRP.Rhine.Schedule (scheduleList)

-- FIXME Don't export Absent, maybe by having an internal module?
data At cl a = Present !a | Absent
  deriving (Show, Eq, Functor, Foldable, Traversable)

currently :: At cl a -> Maybe a
currently (Present a) = Just a
currently Absent = Nothing

instance Applicative (At cl) where
  pure = Present

  Present f <*> Present a = Present $! f a
  Absent <*> Absent = Absent
  _ <*> _ = error "At.<*>: internal error, mixed Absent and Present"

instance Monad (At cl) where
  Present a >>= f = case f a of
    b@(Present _) -> b
    Absent -> error "At.>>=: internal error, mixed Absent and Present"
  Absent >>= _ = Absent

-- FIXME look up how something like this is done properly
-- type family HasClock cl (cls :: [Type]) :: Constraint where
--   HasClock cl (cl ': cls) = ()
--   HasClock cl1 (cl2 ': cls) = HasClock cl1 cls

-- FIXME rewrite with sop-core?
-- FIXME rewrite with prisms?
class HasClock cl cls where
  position :: Position cl cls

instance HasClock cl (cl ': cls) where
  position = Z Refl

instance {-# OVERLAPPABLE #-} (HasClock cl cls) => HasClock cl (cl' ': cls) where
  position = S position

inject :: forall cl cls. (HasClock cl cls) => Proxy cl -> TimeInfo cl -> Tick cls
inject _ = Tick . injectPosition (position @cl @cls)

injectPosition :: Position cl cls -> f cl -> NS f cls
injectPosition (Z Refl) ti = Z ti
injectPosition (S pointer) ti = S $ injectPosition pointer ti

project :: forall cl cls. (HasClock cl cls) => Proxy cl -> Tick cls -> Maybe (TimeInfo cl)
project _ = projectPosition (position @cl @cls) . getTick

projectPosition :: Position cl cls -> NS f cls -> Maybe (f cl)
projectPosition (Z Refl) (Z ti) = Just ti
projectPosition (S position) (S tick) = projectPosition position tick
projectPosition _ _ = Nothing

-- type family HasClocksOrdered clA clB (cls :: [Type]) :: Constraint where
--   HasClocksOrdered clA clB (clA ': cls) = HasClock clB cls
--   HasClocksOrdered clA clB (cl ': cls) = HasClocksOrdered clA clB cls

class HasClocksOrdered clA clB cls where
  orderedPositions :: OrderedPositions clA clB cls

instance (HasClock clB cls) => HasClocksOrdered clA clB (clA ': cls) where
  orderedPositions = OPHere position

instance {-# OVERLAPPABLE #-} (HasClocksOrdered clA clB cls) => HasClocksOrdered clA clB (cl ': cls) where
  orderedPositions = OPThere orderedPositions

firstPosition :: OrderedPositions clA clB cls -> Position clA cls
firstPosition (OPHere _) = Z Refl
firstPosition (OPThere positions) = S $ firstPosition positions

secondPosition :: OrderedPositions clA clB cls -> Position clB cls
secondPosition (OPHere pos) = S pos
secondPosition (OPThere positions) = S $ secondPosition positions

data SNComponent m cls a b where
  Synchronous ::
    (Clock m cl) =>
    Position cl cls ->
    ClSF m cl a b ->
    SNComponent m cls (At cl a) (At cl b)
  Resampling ::
    OrderedPositions clA clB cls ->
    ResamplingBuffer m clA clB a b ->
    SNComponent m cls (At clA a) (At clB b)
  Feedback :: -- FIXME Do I need a particular order for these clocks? Think about some examples
    Position clA cls ->
    Position clB cls ->
    ResamplingBuffer m clA clB a b ->
    SN m cls (At clB b, c) (At clA a, d) ->
    SNComponent m cls c d
  -- FIXME generalise to `forall t . (MonadTrans t, MFunctor t) => ...` to allow e.g. for exception handling
  --   or maybe even arbitrary monads
  Handle :: SListI asbs => (forall r . NP (Curried (Automaton (ReaderT r m))) asbs -> Automaton (ReaderT r m) a b) -> NP (Curried (SN m cls)) asbs -> SNComponent m cls a b
  -- Now :: SNComponent m cls x (Tick cls)
  -- AddClock :: SNComponent m cls a b -> SNComponent m (cl ': cls) a b

newtype Curried f (ab :: (Type, Type)) = Curried { getCurried :: f (Fst ab) (Snd ab)}

type family Fst (ab :: (Type, Type)) :: Type where
  Fst '(a, _) = a

type family Snd (ab :: (Type, Type)) :: Type where
  Snd '(_, b) = b


{- | An 'SN' is a side-effectful asynchronous /__s__ignal __n__etwork/,
where input, data processing (including side effects) and output
need not happen at the same time.

The type parameters are:

* 'm': The monad in which side effects take place.
* 'cl': The clock of the whole signal network.
        It may be sequentially or parallely composed from other clocks.
* 'a': The input type. Input arrives at the rate @In cl@.
* 'b': The output type. Output arrives at the rate @Out cl@.
-}
newtype SN m cls a b = SN {getFreeSN :: A (SNComponent m cls) a b}
  deriving (Category, Arrow)

deriving via (WrappedArrow (SN m cls)) instance Profunctor (SN m cls)

{- | A synchronous automaton is the basic building block.
  For such an 'SN', data enters and leaves the system at the same rate as it is processed.
-}
synchronous :: (HasClock cl cls, Clock m cl) => ClSF m cl a b -> SN m cls (At cl a) (At cl b)
synchronous = SN . liftFree2 . Synchronous position
{-# INLINE synchronous #-}

resampling ::
  ( HasClocksOrdered clA clB cls
  ) =>
  ResamplingBuffer m clA clB a b ->
  SN m cls (At clA a) (At clB b)
resampling = SN . liftFree2 . Resampling orderedPositions

feedbackSN ::
  (HasClock clA cls, HasClock clB cls) =>
  ResamplingBuffer m clA clB a b ->
  SN m cls (At clB b, c) (At clA a, d) ->
  SN m cls c d
feedbackSN sn = SN . liftFree2 . Feedback position position sn

always :: Monad m => Automaton m a b -> SN m cls a b
always automaton = SN $ liftFree2 $ Handle (const $ liftS automaton) Nil

with :: forall m cls a b c d . (forall r . Automaton (ReaderT r m) a b -> Automaton (ReaderT r m) c d) -> SN m cls a b -> SN m cls c d
with morph sn = SN $ liftFree2 $ Handle @('(a, b) ': '[]) (\(Curried automaton :* Nil) -> morph automaton) $ Curried sn :* Nil

handle :: forall m cls a b c d e f . (forall r . Automaton (ReaderT r m) a b -> Automaton (ReaderT r m) c d -> Automaton (ReaderT r m) e f) -> SN m cls a b -> SN m cls c d -> SN m cls e f
handle handler sn1 sn2 = SN $ liftFree2 $ Handle @('(a, b) ': '(c, d) ': '[]) (\(Curried ab :* Curried cd :* Nil) -> handler ab cd) $ Curried sn1 :* Curried sn2 :* Nil

-- now :: SN m cls x (Tick cls)
-- now = SN $ liftFree2 Now

eraseClockSNComponent :: forall m cls a b. (Monad m) => SNComponent m cls a b -> Automaton (ReaderT (Tick cls) m) a b
eraseClockSNComponent (Synchronous position clsf) = readerS $ proc (tick, a) -> do
  case (projectPosition position (getTick tick), a) of
    (Nothing, _) -> returnA -< Absent
    (Just ti, Present a) -> do
      b <- runReaderS clsf -< (ti, a)
      returnA -< Present b
    (Just _ti, Absent) -> returnA -< error "eraseClockSNComponent: Internal error (Synchronous)"
eraseClockSNComponent (Resampling positions resbuf0) = readerS $ eraseClockResBuf (Proxy @cls) positions resbuf0
eraseClockSNComponent (Feedback posA posB ResamplingBuffer {buffer = buffer0, get, put} sn) =
  let
    snErased = runReaderS $ eraseClockFreeSN sn
   in
    readerS $ feedback buffer0 $ proc ((tick, a), buffer) -> do
      (bAt, buffer') <- case projectPosition posB $ getTick tick of
        Nothing -> returnA -< (Absent, buffer)
        Just ti -> do
          Result buffer' b <- arrM $ uncurry get -< (ti, buffer)
          returnA -< (Present b, buffer')
      (aAt, b) <- snErased -< (tick, (bAt, a))
      buffer'' <- case (projectPosition posA $ getTick tick, aAt) of
        (Nothing, _) -> returnA -< buffer'
        (Just ti, Present a) -> do
          arrM $ uncurry $ uncurry put -< ((ti, a), buffer')
        _ -> returnA -< error "eraseClockSNComponent: internal error (Feedback)"
      returnA -< (b, buffer'')
eraseClockSNComponent (Handle handler sns) = handler $ hmap (Curried . eraseClockFreeSN . getCurried) sns
-- eraseClockSNComponent Now = constM ask

eraseClockResBuf ::
  (Monad m) =>
  Proxy cls ->
  OrderedPositions clA clB cls ->
  ResamplingBuffer m clA clB a1 a2 ->
  Automaton m (Tick cls, At clA a1) (At clB a2)
eraseClockResBuf _ orderedPositions ResamplingBuffer {buffer = buffer0, put, get} =
  let
    posIn = firstPosition orderedPositions
    posOut = secondPosition orderedPositions
   in
    feedback buffer0 $ proc ((tick, a), buffer) -> do
      buffer' <- case (projectPosition posIn $ getTick tick, a) of
        (Nothing, _) -> returnA -< buffer
        (Just ti, Present a) -> do
          arrM $ uncurry $ uncurry put -< ((ti, a), buffer)
        _ -> returnA -< error "eraseClockResBuf: internal error"
      case projectPosition posOut $ getTick tick of
        Nothing -> returnA -< (Absent, buffer')
        Just ti -> do
          Result buffer'' b <- arrM $ uncurry get -< (ti, buffer')
          returnA -< (Present b, buffer'')

proxyFromClSF :: ClSF m cl a b -> Proxy cl
proxyFromClSF _ = Proxy

proxyInFromResBuf :: ResamplingBuffer m clA clB a b -> Proxy clA
proxyInFromResBuf _ = Proxy

proxyOutFromResBuf :: ResamplingBuffer m clA clB a b -> Proxy clB
proxyOutFromResBuf _ = Proxy

eraseClockFreeSN :: (Monad m) => SN m cls a b -> Automaton (ReaderT (Tick cls) m) a b
eraseClockFreeSN SN {getFreeSN} = runA getFreeSN eraseClockSNComponent

-- eraseClockFreeSN' :: (Monad m) => SN m cls a b -> ClSF m (Clocks m td cls) a b
-- eraseClockFreeSN' = morphS (withReaderT _) . eraseClockFreeSN

-- FIXME interesting idea: Erase only some clocks, e.g. the first one of the stack.
-- Then I need a concept between SN and Automaton.
-- The advantage would be higher flexibility, and I could maye also use MonadSchedule to make the data parts concurrent

infixr 9 .:.

(.:.) :: (GetClockProxy cl, Clock m cl) => cl -> Clocks m (Time cl) cls -> Clocks m (Time cl) (cl ': cls)
getClassyClock .:. Clocks {getClocks} = Clocks $ ClassyClock {getClassyClock} :* getClocks

clocks :: (GetClockProxy cl, Clock m cl) => cl -> Clocks m (Time cl) '[cl]
clocks cl = cl .:. cnil

cnil :: Clocks m td '[]
cnil = Clocks Nil

data ClassyClock m td cl where
  ClassyClock :: (Clock m cl, GetClockProxy cl, Time cl ~ td) => {getClassyClock :: cl} -> ClassyClock m td cl

-- FIXME This is
newtype Clocks m td cls = Clocks {getClocks :: NP (ClassyClock m td) cls}

type Position cl cls = NS ((:~:) cl) cls

data OrderedPositions cl1 cl2 cls where
  OPHere :: Position cl2 cls -> OrderedPositions cl1 cl2 (cl1 ': cls)
  OPThere :: OrderedPositions cl1 cl2 cls -> OrderedPositions cl1 cl2 (cl ': cls)

newtype Tick cls = Tick {getTick :: NS TimeInfo cls}

type family Append (cls1 :: [Type]) (cls2 :: [Type]) :: [Type] where
  Append '[] cls = cls
  Append (cl ': cls1) cls2 = cl ': Append cls1 cls2

appendPosition :: Clocks m td cls2 -> Position cl cls1 -> Position cl (Append cls1 cls2)
appendPosition _ (Z Refl) = Z Refl
appendPosition clocks (S pos) = S $ appendPosition clocks pos

prependPosition :: Clocks m td cls1 -> Position cl cls2 -> Position cl (Append cls1 cls2)
prependPosition Clocks {getClocks = Nil} pos = pos
prependPosition Clocks {getClocks = _ :* getClocks} pos = S $ prependPosition Clocks {getClocks} pos

appendPositions :: Clocks m td cls2 -> OrderedPositions clA clB cls1 -> OrderedPositions clA clB (Append cls1 cls2)
appendPositions clocks (OPHere pos) = OPHere $ appendPosition clocks pos
appendPositions clocks (OPThere positions) = OPThere $ appendPositions clocks positions

appendClocks :: Clocks m td cls1 -> Clocks m td cls2 -> Clocks m td (Append cls1 cls2)
appendClocks Clocks {getClocks = Nil} clocks = clocks
appendClocks Clocks {getClocks = cl :* cls} clocks =
  let Clocks {getClocks} = appendClocks Clocks {getClocks = cls} clocks
   in Clocks {getClocks = cl :* getClocks}

addClockSNComponent :: SNComponent m cls a b -> SNComponent m (cl ': cls) a b
addClockSNComponent (Synchronous position clsf) = Synchronous (S position) clsf
addClockSNComponent (Resampling positions clsf) = Resampling (OPThere positions) clsf
addClockSNComponent (Feedback posA posB resbuf sn) = Feedback (S posA) (S posB) resbuf (addClockSN sn)
addClockSNComponent (Handle f sns) = Handle f $ hmap (Curried . addClockSN . getCurried) sns
-- addClockSNComponent Now = liftFree2 Now

appendClockSNComponent :: Clocks m td cls2 -> SNComponent m cls1 a b -> SNComponent m (Append cls1 cls2) a b
appendClockSNComponent clocks (Synchronous position clsf) = Synchronous (appendPosition clocks position) clsf
appendClockSNComponent clocks (Resampling positions resbuf) = Resampling (appendPositions clocks positions) resbuf
appendClockSNComponent clocks (Feedback posA posB resbuf sn) =
  Feedback
    (appendPosition clocks posA)
    (appendPosition clocks posB)
    resbuf
    (appendClocksSN clocks sn)
appendClockSNComponent clocks (Handle f sns) = Handle f $ hmap (Curried . appendClocksSN clocks . getCurried) sns

addClockSN :: SN m cls a b -> SN m (cl ': cls) a b
addClockSN = SN . foldNatFree2 (liftFree2 . addClockSNComponent) . getFreeSN

prependClocksSN :: Clocks m td cls1 -> SN m cls2 a b -> SN m (Append cls1 cls2) a b
prependClocksSN Clocks {getClocks = Nil} = id
prependClocksSN Clocks {getClocks = _ :* getClocks} = addClockSN . prependClocksSN Clocks {getClocks}

appendClocksSN :: Clocks m td cls2 -> SN m cls1 a b -> SN m (Append cls1 cls2) a b
appendClocksSN clocks = SN . foldNatFree2 (liftFree2 . appendClockSNComponent clocks) . getFreeSN

orderedPositionsInAppend ::
  Clocks m td cls1 ->
  Clocks m td cls2 ->
  Position cl1 cls1 ->
  Position cl2 cls2 ->
  OrderedPositions cl1 cl2 (Append cls1 cls2)
orderedPositionsInAppend Clocks {getClocks = _ :* getClocks} _ (Z Refl) pos2 = OPHere $ prependPosition Clocks {getClocks} pos2
orderedPositionsInAppend Clocks {getClocks = _ :* getClocks} cls2 (S pos1) pos2 = OPThere $ orderedPositionsInAppend Clocks {getClocks} cls2 pos1 pos2
-- I think that there are no other valid patterns. GHC 9.4 is unsure about that because of https://gitlab.haskell.org/ghc/ghc/-/issues/22684.
-- Revisit with GHC 9.6.
orderedPositionsInAppend Clocks {getClocks = Nil} _ _ _ = error "orderedPositionsInAppend: Internal error. Please report as a rhine bug."

runClocks :: (Monad m, MonadSchedule m) => Clocks m td cls -> Automaton m () (Tick cls)
runClocks cls = performOnFirstSample $ scheduleAutomatons <$> getRunningClocks (getClocks cls)
  where
    getRunningClocks :: (Monad m) => NP (ClassyClock m td) cls -> m [Automaton m () (Tick cls)]
    getRunningClocks Nil = pure []
    getRunningClocks (cl :* cls) = (:) <$> startAndInjectClock cl <*> (map (>>> arr (Tick . S . getTick)) <$> getRunningClocks cls)

    startAndInjectClock :: (Monad m, HasClock cl cls) => ClassyClock m td cl -> m (Automaton m () (Tick cls))
    startAndInjectClock (ClassyClock cl) = do
      (runningClock, initTime) <- initClock cl
      return $ runningClock >>> genTimeInfo getClockProxy initTime >>> arr (inject (clockProxy cl))

    clockProxy :: cl -> Proxy cl
    clockProxy _ = Proxy

    scheduleAutomatons :: (Monad m, MonadSchedule m) => [Automaton m () a] -> Automaton m () a
    scheduleAutomatons msfs = concatS $ scheduleList (fromList msfs) >>> arr toList

newtype AtAny cls a = AtAny {getAtAny :: Maybe (AtSome cls a)}
newtype AtSome cls a = AtSome {getAtSome :: NS (K a) cls}

atAny :: AtSome cls a -> AtAny cls a
atAny = AtAny . Just

currentlyAny :: AtAny cls a -> Maybe (AtSome cls a)
currentlyAny = getAtAny

-- This might go to a separate module, or maybe there is a helper library? Do I need this elsewhere? Is this in SOP?
newtype Flip f a b = Flip {getFlip :: f b a}

atThis :: forall cl cls a . HasClock cl cls => At cl a -> AtAny cls a
atThis cla@(Present a) = AtAny $ Just $ AtSome $ injectPosition (position @cl) $ K a
atThis Absent = AtAny Nothing
