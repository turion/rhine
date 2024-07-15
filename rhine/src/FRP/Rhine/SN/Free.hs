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

module FRP.Rhine.SN.Free (
  At (
    -- It is intentional _not_ to export the constructor Absent.
    -- It may only be used internally to guarantee clock safety,
    -- since otherwise a user could create an absent signal
    -- in a situation where the clock ticks.
    -- The constructor Present is harmless though, since an unneeded value is simply discarded.
    Present
  ),
  SNComponent (..),
  FreeSN (..),
  eraseClockFreeSN,
  synchronous,
  resampling,
  feedbackSN,
  always,
  with,
  handle,
  currently,
  Clocks (..),
  NP (..),
  NS (..),
  (.:.),
  cnil,
  (^>>>),
  (>>>^),
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
)
where

-- FIXME sort imports and exports

import Control.Arrow.Free
import Control.Category (Category)
import Control.Monad.Schedule.Class (MonadSchedule)
import Data.Automaton.Trans.Except (performOnFirstSample)
import Data.Automaton.Trans.Reader (readerS, runReaderS)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Kind (Type)
import Data.List.NonEmpty (fromList, toList)
import Data.Automaton (concatS)
import Data.Proxy (Proxy (..))
import Data.SOP (NP (..), NS (..))
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
    FreeSN m cls (At clB b, c) (At clA a, d) ->
    SNComponent m cls c d
  Always ::
    MSF m a b -> SNComponent m cls a b
  With ::
    (forall r . MSF (ReaderT r m) a b -> MSF (ReaderT r m) c d) ->
    FreeSN m cls a b ->
    SNComponent m cls c d
  Handle ::
    (forall r . MSF (ReaderT r m) a b -> MSF (ReaderT r m) c d -> MSF (ReaderT r m) e f) ->
    FreeSN m cls a b ->
    FreeSN m cls c d ->
    SNComponent m cls e f
  -- FIXME generalise to a NP of arguments, but I don't know how I zip type level lists
  -- FIXME generalise to `forall t . (MonadTrans t, MFunctor t) => ...` to allow e.g. for exception handling
  --   or maybe even arbitrary monads

newtype FreeSN m cls a b = FreeSN {getFreeSN :: A (SNComponent m cls) a b}
  deriving (Category, Arrow)

deriving via (WrappedArrow (FreeSN m cls)) instance Profunctor (FreeSN m cls)

synchronous :: (HasClock cl cls, Clock m cl) => ClSF m cl a b -> FreeSN m cls (At cl a) (At cl b)
synchronous = FreeSN . liftFree2 . Synchronous position

resampling ::
  ( HasClocksOrdered clA clB cls
  ) =>
  ResamplingBuffer m clA clB a b ->
  FreeSN m cls (At clA a) (At clB b)
resampling = FreeSN . liftFree2 . Resampling orderedPositions

feedbackSN ::
  (HasClock clA cls, HasClock clB cls) =>
  ResamplingBuffer m clA clB a b ->
  FreeSN m cls (At clB b, c) (At clA a, d) ->
  FreeSN m cls c d
feedbackSN sn = FreeSN . liftFree2 . Feedback position position sn

always :: MSF m a b -> FreeSN m cls a b
always = FreeSN . liftFree2 . Always

with :: (forall r . MSF (ReaderT r m) a b -> MSF (ReaderT r m) c d) -> FreeSN m cls a b -> FreeSN m cls c d
with morph = FreeSN . liftFree2 . With morph

handle :: (forall r . MSF (ReaderT r m) a b -> MSF (ReaderT r m) c d -> MSF (ReaderT r m) e f) -> FreeSN m cls a b -> FreeSN m cls c d -> FreeSN m cls e f
handle handler sn1 sn2 = FreeSN $ liftFree2 $ Handle handler sn1 sn2

eraseClockSNComponent :: forall m cls a b. (Monad m) => SNComponent m cls a b -> MSF (ReaderT (Tick cls) m) a b
eraseClockSNComponent (Synchronous position clsf) = readerS $ proc (tick, a) -> do
  case (projectPosition position (getTick tick), a) of
    (Nothing, _) -> returnA -< Absent
    (Just ti, Present a) -> do
      b <- runReaderS clsf -< (ti, a)
      returnA -< Present b
    _ -> error "eraseClockSNComponent: Internal error (Synchronous)" -< ()
eraseClockSNComponent (Resampling positions resbuf0) = readerS $ eraseClockResBuf (Proxy @cls) positions resbuf0
eraseClockSNComponent (Feedback posA posB resbuf0 sn) =
  let
    snErased = runReaderS $ eraseClockFreeSN sn
   in
    readerS $ feedback resbuf0 $ proc ((tick, a), resbuf) -> do
      (bAt, resbuf') <- case projectPosition posB $ getTick tick of
        Nothing -> returnA -< (Absent, resbuf)
        Just ti -> do
          (b, resbuf') <- arrM $ uncurry get -< (resbuf, ti)
          returnA -< (Present b, resbuf')
      (aAt, b) <- snErased -< (tick, (bAt, a))
      resbuf'' <- case (projectPosition posA $ getTick tick, aAt) of
        (Nothing, _) -> returnA -< resbuf'
        (Just ti, Present a) -> do
          arrM $ uncurry $ uncurry put -< ((resbuf', ti), a)
        _ -> error "eraseClockSNComponent: internal error (Feedback)" -< ()
      returnA -< (b, resbuf'')
eraseClockSNComponent (Always msf) = liftTransS msf
eraseClockSNComponent (With morph sn) = morph $ eraseClockFreeSN sn
eraseClockSNComponent (Handle handler sn1 sn2)= handler (eraseClockFreeSN sn1) (eraseClockFreeSN sn2)

eraseClockResBuf ::
  (Monad m) =>
  Proxy cls ->
  OrderedPositions clA clB cls ->
  ResamplingBuffer m clA clB a1 a2 ->
  MSF m (Tick cls, At clA a1) (At clB a2)
eraseClockResBuf _ orderedPositions resbuf0 =
  let
    posIn = firstPosition orderedPositions
    posOut = secondPosition orderedPositions
   in
    feedback resbuf0 $ proc ((tick, a), resbuf) -> do
      resbuf' <- case (projectPosition posIn $ getTick tick, a) of
        (Nothing, _) -> returnA -< resbuf
        (Just ti, Present a) -> do
          arrM $ uncurry $ uncurry put -< ((resbuf, ti), a)
        _ -> error "eraseClockResBuf: internal error" -< ()
      case projectPosition posOut $ getTick tick of
        Nothing -> returnA -< (Absent, resbuf')
        Just ti -> do
          (b, resbuf'') <- arrM $ uncurry get -< (resbuf', ti)
          returnA -< (Present b, resbuf'')

proxyFromClSF :: ClSF m cl a b -> Proxy cl
proxyFromClSF _ = Proxy

proxyInFromResBuf :: ResamplingBuffer m clA clB a b -> Proxy clA
proxyInFromResBuf _ = Proxy

proxyOutFromResBuf :: ResamplingBuffer m clA clB a b -> Proxy clB
proxyOutFromResBuf _ = Proxy

eraseClockFreeSN :: (Monad m) => FreeSN m cls a b -> MSF (ReaderT (Tick cls) m) a b
eraseClockFreeSN FreeSN {getFreeSN} = runA getFreeSN eraseClockSNComponent

-- eraseClockFreeSN' :: (Monad m) => FreeSN m cls a b -> ClSF m (Clocks m td cls) a b
-- eraseClockFreeSN' = morphS (withReaderT _) . eraseClockFreeSN

-- FIXME interesting idea: Erase only some clocks, e.g. the first one of the stack.
-- Then I need a concept between FreeSN and MSF.
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
addClockSNComponent (Always msf) = Always msf

appendClockSNComponent :: Clocks m td cls2 -> SNComponent m cls1 a b -> SNComponent m (Append cls1 cls2) a b
appendClockSNComponent clocks (Synchronous position clsf) = Synchronous (appendPosition clocks position) clsf
appendClockSNComponent clocks (Resampling positions resbuf) = Resampling (appendPositions clocks positions) resbuf
appendClockSNComponent clocks (Feedback posA posB resbuf sn) =
  Feedback
    (appendPosition clocks posA)
    (appendPosition clocks posB)
    resbuf
    (appendClocksSN clocks sn)
appendClockSNComponent _ (Always msf) = Always msf

addClockSN :: FreeSN m cls a b -> FreeSN m (cl ': cls) a b
addClockSN = FreeSN . foldNatFree2 (liftFree2 . addClockSNComponent) . getFreeSN

prependClocksSN :: Clocks m td cls1 -> FreeSN m cls2 a b -> FreeSN m (Append cls1 cls2) a b
prependClocksSN Clocks {getClocks = Nil} = id
prependClocksSN Clocks {getClocks = _ :* getClocks} = addClockSN . prependClocksSN Clocks {getClocks}

appendClocksSN :: Clocks m td cls2 -> FreeSN m cls1 a b -> FreeSN m (Append cls1 cls2) a b
appendClocksSN clocks = FreeSN . foldNatFree2 (liftFree2 . appendClockSNComponent clocks) . getFreeSN

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

runClocks :: (Monad m, MonadSchedule m) => Clocks m td cls -> MSF m () (Tick cls)
runClocks cls = performOnFirstSample $ scheduleMSFs <$> getRunningClocks (getClocks cls)
  where
    getRunningClocks :: (Monad m) => NP (ClassyClock m td) cls -> m [MSF m () (Tick cls)]
    getRunningClocks Nil = pure []
    getRunningClocks (cl :* cls) = (:) <$> startAndInjectClock cl <*> (map (>>> arr (Tick . S . getTick)) <$> getRunningClocks cls)

    startAndInjectClock :: (Monad m, HasClock cl cls) => ClassyClock m td cl -> m (MSF m () (Tick cls))
    startAndInjectClock (ClassyClock cl) = do
      (runningClock, initTime) <- initClock cl
      return $ runningClock >>> genTimeInfo getClockProxy initTime >>> arr (inject (clockProxy cl))

    clockProxy :: cl -> Proxy cl
    clockProxy _ = Proxy

    scheduleMSFs :: (Monad m, MonadSchedule m) => [MSF m () a] -> MSF m () a
    scheduleMSFs msfs = concatS $ scheduleList (fromList msfs) >>> arr toList

infix 4 >>>^

-- | Operator alias for 'rmap', useful to postcompose a 'Rhine' or 'SN' with a function
(>>>^) :: (Profunctor p) => p a b -> (b -> c) -> p a c
(>>>^) = flip rmap

infix 3 ^>>>

-- | Operator alias for 'lmap', useful to precompose a 'Rhine' or 'SN' with a function
(^>>>) :: (Profunctor p) => (a -> b) -> p b c -> p a c
(^>>>) = lmap
