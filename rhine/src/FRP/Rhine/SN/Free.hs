{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FRP.Rhine.SN.Free (
  At (
    -- It is intentional _not_ to export the constructor Absent.
    -- It may only be used internally to guarantee clock safety,
    -- since otherwise a user could create an absent signal
    -- in a situation where the clock ticks.
    -- The constructor Present is harmless though, since an unneeded value is simply discarded.
    Present
  ),
  eraseClockFreeSN,
  synchronous,
  resampling,
  feedbackSN,
  always,
  currently,
  Rhine(..),
  eraseClockRhine,
  flow,
  Clocks(..),
  NP(..),
  NS(..),
  (.:.),
  cnil
)
where

import Control.Arrow.Free
import Control.Monad.Trans.MSF.Reader (readerS, runReaderS)
import Control.Monad.Trans.Reader (ReaderT, withReaderT)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock (Clock (..), TimeDomain, TimeInfo (..), tag)
import FRP.Rhine.ResamplingBuffer (ResamplingBuffer (..))
import FRP.Rhine.Clock.Util (genTimeInfo)
import FRP.Rhine.Clock.Proxy (GetClockProxy(getClockProxy), toClockProxy, ToClockProxy)
import FRP.Rhine.Schedule (scheduleList)
import Data.List.NonEmpty (fromList, toList)
import Control.Monad.Schedule.Class (MonadSchedule)
import Data.MonadicStreamFunction.Async (concatS)
import Control.Monad.Trans.MSF (performOnFirstSample)
import Control.Category (Category)
import Data.Type.Equality ((:~:) (Refl))
import Data.Typeable (cast, Typeable)
import Generics.SOP (NS (..), NP (..))

-- Don't export Absent
data At cl a = Present !a | Absent

currently :: At cl a -> Maybe a
currently (Present a) = Just a
currently Absent = Nothing

instance Functor (At cl) where
  fmap f (Present a) = Present $ f a
  fmap _ Absent = Absent

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

-- FIXME rewrite with prisms?
class HasClock cl cls where
  position :: Position cl cls

instance HasClock cl (cl ': cls) where
  position = Z Refl

instance {-# OVERLAPPABLE #-} (HasClock cl cls) => HasClock cl (cl' ': cls) where
  position = S position

inject :: forall cl cls . HasClock cl cls => Proxy cl -> TimeInfo cl -> Tick cls
inject _ = Tick . injectPosition (position @cl @cls)

injectPosition :: Position cl cls -> f cl -> NS f cls
injectPosition (Z Refl) ti = Z ti
injectPosition (S pointer) ti = S $ injectPosition pointer ti

project :: forall cl cls . HasClock cl cls => Proxy cl -> Tick cls -> Maybe (TimeInfo cl)
project _ = projectPosition (position @cl @cls) . getTick

projectPosition :: Position cl cls -> NS f cls -> Maybe (f cl)
projectPosition (Z Refl) (Z ti) = Just ti
projectPosition (S position) (S tick) = projectPosition position tick
projectPosition _ _ = Nothing


type family HasClocksOrdered clA clB (cls :: [Type]) :: Constraint where
  HasClocksOrdered clA clB (clA ': cls) = HasClock clB cls
  HasClocksOrdered clA clB (cl ': cls) = HasClocksOrdered clA clB cls

data SNComponent m cls a b where
  Synchronous ::
    (HasClock cl cls, Clock m cl) =>
    ClSF m cl a b ->
    SNComponent m cls (At cl a) (At cl b)
  Resampling ::
    ( HasClocksOrdered clA clB cls
    , HasClock clA cls
    , HasClock clB cls -- FIXME The first constraint implies the second and third
    ) =>
    ResamplingBuffer m clA clB a b ->
    SNComponent m cls (At clA a) (At clB b)
  Feedback :: -- FIXME Do I need a particular order for these clocks? Think about some examples
    (HasClock clA cls, HasClock clB cls) =>
    FreeSN m cls (At clB b, c) (At clA a, d) ->
    ResamplingBuffer m clA clB a b ->
    SNComponent m cls c d
  Always ::
    MSF m a b -> SNComponent m cls a b

newtype FreeSN m cls a b = FreeSN {getFreeSN :: A (SNComponent m cls) a b}
  deriving (Category, Arrow)

synchronous :: (HasClock cl cls, Clock m cl) => ClSF m cl a b -> FreeSN m cls (At cl a) (At cl b)
synchronous = FreeSN . liftFree2 . Synchronous

resampling ::
  ( HasClock clA cls
  , Clock m clA
  , HasClocksOrdered clA clB cls
  , HasClock clB cls
  ) =>
  ResamplingBuffer m clA clB a b ->
  FreeSN m cls (At clA a) (At clB b)
resampling = FreeSN . liftFree2 . Resampling

feedbackSN ::
  (HasClock clA cls, HasClock clB cls) =>
  FreeSN m cls (At clB b, c) (At clA a, d) ->
  ResamplingBuffer m clA clB a b ->
  FreeSN m cls c d
feedbackSN sn = FreeSN . liftFree2 . Feedback sn

always :: MSF m a b -> FreeSN m cls a b
always = FreeSN . liftFree2 . Always

eraseClockSNComponent :: forall m cls a b. (Monad m) => SNComponent m cls a b -> MSF (ReaderT (Tick cls) m) a b
eraseClockSNComponent (Synchronous clsf) = readerS $ proc (tick, a) -> do
  case (project (proxyFromClSF clsf) tick, a) of
    (Nothing, _) -> returnA -< Absent
    (Just ti, Present a) -> do
      b <- runReaderS clsf -< (ti, a)
      returnA -< Present b
    _ -> error "eraseClockSNComponent: Internal error (Synchronous)" -< ()
eraseClockSNComponent (Resampling resbuf0) = readerS $ eraseClockResBuf (Proxy @cls) resbuf0
eraseClockSNComponent (Feedback sn resbuf0) =
  let
    proxyIn = proxyInFromResBuf resbuf0
    proxyOut = proxyOutFromResBuf resbuf0
    snErased = runReaderS $ eraseClockFreeSN sn
   in
    readerS $ feedback resbuf0 $ proc ((tick, a), resbuf) -> do
      (bAt, resbuf') <- case project proxyOut tick of
        Nothing -> returnA -< (Absent, resbuf)
        Just ti -> do
          (b, resbuf') <- arrM $ uncurry get -< (resbuf, ti)
          returnA -< (Present b, resbuf')
      (aAt, b) <- snErased -< (tick, (bAt, a))
      resbuf'' <- case (project proxyIn tick, aAt) of
        (Nothing, _) -> returnA -< resbuf'
        (Just ti, Present a) -> do
          arrM $ uncurry $ uncurry put -< ((resbuf', ti), a)
        _ -> error "eraseClockSNComponent: internal error (Feedback)" -< ()
      returnA -< (b, resbuf'')
eraseClockSNComponent (Always msf) = liftTransS msf

eraseClockResBuf ::
  (Monad m, HasClock cla cls, HasClock clb cls) =>
  Proxy cls ->
  ResamplingBuffer m cla clb a1 a2 ->
  MSF m (Tick cls, At cl1 a1) (At cl2 a2)
eraseClockResBuf _ resbuf0 =
  let
    proxyIn = proxyInFromResBuf resbuf0
    proxyOut = proxyOutFromResBuf resbuf0
   in
    feedback resbuf0 $ proc ((tick, a), resbuf) -> do
      resbuf' <- case (project proxyIn tick, a) of
        (Nothing, _) -> returnA -< resbuf
        (Just ti, Present a) -> do
          arrM $ uncurry $ uncurry put -< ((resbuf, ti), a)
        _ -> error "eraseClockSNComponent: internal error (Resampling)" -< ()
      case project proxyOut tick of
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

infixr .:.

(.:.) :: (GetClockProxy cl, Clock m cl) => cl -> Clocks m (Time cl) cls -> Clocks m (Time cl) (cl ': cls)
getClassyClock .:. Clocks {getClocks} = Clocks $ ClassyClock {getClassyClock} :* getClocks

cnil :: Clocks m td '[]
cnil = Clocks Nil

data ClassyClock m td cl where
  ClassyClock :: (Clock m cl, GetClockProxy cl, Time cl ~ td) => {getClassyClock :: cl} -> ClassyClock m td cl

-- FIXME This is
newtype Clocks m td cls = Clocks {getClocks :: NP (ClassyClock m td) cls}

type Position cl cls = NS ((:~:) cl) cls

newtype Tick cls = Tick {getTick :: NS TimeInfo cls}

data Rhine m td cls a b = Rhine
  { clocks :: Clocks m td cls
  , sn :: FreeSN m cls a b
  }

eraseClockRhine :: (Monad m, MonadSchedule m) => Rhine m td cls a b -> MSF m a b
eraseClockRhine Rhine {clocks, sn} = proc a -> do
  ti <- runClocks clocks -< ()
  runReaderS (eraseClockFreeSN sn) -< (ti, a)

flow :: (Monad m, MonadSchedule m) => Rhine m td cls () () -> m ()
flow = reactimate . eraseClockRhine

runClocks :: (Monad m, MonadSchedule m) => Clocks m td cls -> MSF m () (Tick cls)
runClocks cls = performOnFirstSample $ scheduleMSFs <$> getRunningClocks (getClocks cls)
 where
  getRunningClocks :: Monad m => NP (ClassyClock m td) cls -> m [MSF m () (Tick cls)]
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
