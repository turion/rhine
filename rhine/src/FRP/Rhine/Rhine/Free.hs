{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

module FRP.Rhine.Rhine.Free where

import FRP.Rhine.SN.Free
import Data.Profunctor
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ResamplingBuffer
import Control.Monad.Schedule.Class
import Control.Arrow.Free
import Control.Monad.Trans.MSF.Reader (runReaderS)

data Rhine m td cls a b = Rhine
  { clocks :: Clocks m td cls
  , sn :: FreeSN m cls a b
  }

instance Profunctor (Rhine m td cls) where
  dimap f g Rhine {clocks, sn} =
    Rhine
      { clocks
      , sn = dimap f g sn
      }

(>@>) :: Rhine m td cls1 a b -> Rhine m td cls2 b c -> Rhine m td (Append cls1 cls2) a c
Rhine clocks1 sn1 >@> Rhine clocks2 sn2 =
  let clocks = appendClocks clocks1 clocks2
   in Rhine
        { clocks
        , sn = appendClocksSN clocks2 sn1 >>> prependClocksSN clocks1 sn2
        }

infix 5 @@
(@@) :: (Clock m cl, GetClockProxy cl) => ClSF m cl a b -> cl -> Rhine m (Time cl) '[cl] (At cl a) (At cl b)
clsf @@ cl =
  Rhine
    { clocks = Clocks {getClocks = ClassyClock cl :* Nil}
    , sn = synchronous clsf
    }

data RhineAndResamplingBuffer m td cls clC a c
  = forall clB b.
    (Clock m clB) =>
    RhineAndResamplingBuffer (Position clB cls) (Rhine m td cls a (At clB b)) (ResamplingBuffer m clB clC b c)

infix 2 >--
(>--) :: (Clock m clB, HasClock clB cls) => Rhine m td cls a (At clB b) -> ResamplingBuffer m clB clC b c -> RhineAndResamplingBuffer m td cls clC a c
(>--) = RhineAndResamplingBuffer position

infixr 1 -->
(-->) :: (HasClock clC cls2) => RhineAndResamplingBuffer m td cls1 clC a c -> Rhine m td cls2 (At clC c) d -> Rhine m td (Append cls1 cls2) a d
RhineAndResamplingBuffer positionB (Rhine cls1 sn1) rb --> Rhine cls2 sn2 =
  let positionC = position
   in Rhine
        { clocks = appendClocks cls1 cls2
        , sn =
            appendClocksSN cls2 sn1
              >>> FreeSN (liftFree2 (Resampling (orderedPositionsInAppend cls1 cls2 positionB positionC) rb))
              >>> prependClocksSN cls1 sn2
        }

eraseClockRhine :: (Monad m, MonadSchedule m) => Rhine m td cls a b -> MSF m a b
eraseClockRhine Rhine {clocks, sn} = proc a -> do
  ti <- runClocks clocks -< ()
  runReaderS (eraseClockFreeSN sn) -< (ti, a)

flow :: (Monad m, MonadSchedule m) => Rhine m td cls () a -> m ()
flow = reactimate . eraseClockRhine . (>>>^ const ())

infix 2 *@*
(*@*) :: Rhine m td cls1 a b -> Rhine m td cls2 c d -> Rhine m td (Append cls1 cls2) (a, c) (b, d)
Rhine cls1 sn1 *@* Rhine cls2 sn2 =
  Rhine
    { clocks = appendClocks cls1 cls2
    , sn = appendClocksSN cls2 sn1 *** prependClocksSN cls1 sn2
    }
