{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module FRP.Rhine.SN.Free (At (Present), eraseClockSNComponent)
where

import Control.Arrow.Free
import Control.Monad.Trans.MSF.Reader (runReaderS, readerS)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import FRP.Rhine (ResamplingBuffer, TimeInfo (TimeInfo))
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock (Clock (..), TimeDomain, TimeInfo, tag)
import FRP.Rhine.ResamplingBuffer (ResamplingBuffer (..))
import Control.Category (Category)
import Control.Monad.Trans.Reader (ReaderT, withReaderT)

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
  inject :: Proxy cl -> TimeInfo cl -> Tick cls
  project :: Proxy cl -> Tick cls -> Maybe (TimeInfo cl)

instance HasClock cl (cl ': cls) where
  inject _ = Here
  project _ (Here ti) = Just ti
  project _ _ = Nothing

instance (HasClock cl cls) => HasClock cl (cl' ': cls) where
  inject proxy ti = There $ inject proxy ti
  project _ (Here _) = Nothing
  project proxy (There tick) = project proxy tick

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
  Wild :: -- FIXME Do I need this really? It's like a general buffer
    MSF m a b -> SNComponent m cls a b

newtype FreeSN m cls a b = FreeSN {getFreeSN :: A (SNComponent m cls) a b}

eraseClockSNComponent :: forall m cls a b . (Monad m) => SNComponent m cls a b -> MSF (ReaderT (Tick cls) m) a b
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
        _ -> error "eraseClockSNComponent: internal error (Resampling)" -< ()
      returnA -< (b, resbuf'')

eraseClockSNComponent (Wild msf) = liftTransS msf

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

eraseClockFreeSN :: Monad m => FreeSN m cls a b -> MSF (ReaderT (Tick cls) m) a b
eraseClockFreeSN FreeSN {getFreeSN} = runA getFreeSN eraseClockSNComponent

eraseClockFreeSN' :: Monad m => FreeSN m cls a b -> ClSF m (Clocks m td cls) a b
eraseClockFreeSN' = morphS (withReaderT _) . eraseClockFreeSN

type family Map (f :: Type -> Type) (ts :: [Type]) :: [Type] where
  Map f '[] = '[]
  Map f (t ': ts) = f t ': Map f ts

data HTuple cls where
  Unit :: cl -> HTuple '[cl]
  Cons :: cl -> HTuple cls -> HTuple (cl ': cls)

data ClassyClock m td cl where
  ClassyClock :: (Clock m cl, Time cl ~ td) => cl -> ClassyClock m td cl

-- FIXME maybe put Clock constraints and time domain here?
-- data Clocks m td cls where
--   UnitClock :: (Clock m cl, Time cl ~ td) => cl -> Clocks m td '[cl]
--   ConsClocks :: (Clock m cl, Time cl ~ td) => cl -> Clocks m td cls -> Clocks m td (cl ': cls)
newtype Clocks m td cls = Clocks {getClocks :: HTuple (Map (ClassyClock m td) cls)}

data Tick cls where
  Here :: TimeInfo cl -> Tick (cl ': cls)
  There :: Tick cls -> Tick (cl ': cls)
