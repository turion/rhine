{- |
Combinators for composing signal networks sequentially and parallely.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module FRP.Rhine.SN.Combinators where


-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ResamplingBuffer.Util
import FRP.Rhine.Schedule
import FRP.Rhine.SN
import FRP.Rhine.ResamplingBuffer


-- | Postcompose a signal network with a pure function.
(>>>^)
  :: Monad m
  => SN m cla clb a b
  ->               (b -> b')
  -> SN m cla clb a      b'
Synchronous clsf cl      >>>^ f = Synchronous (clsf >>^ f) cl
Sequential sn rb cl clsf >>>^ f = Sequential sn rb cl $ clsf >>^ f
Parallel   snL    snR    >>>^ f = Parallel  (snL >>>^ f) (snR >>>^ f)


-- | Precompose a signal network with a pure function.
(^>>>)
  :: Monad m
  =>             (a -> a')
  -> SN m cla clb      a' b
  -> SN m cla clb a       b
f ^>>> Synchronous clsf cl      = Synchronous (f ^>>  clsf) cl
f ^>>> Sequential sn rb cl clsf = Sequential  (f ^>>> sn) rb cl clsf
f ^>>> Parallel snL snR         = Parallel    (f ^>>> snL) (f ^>>> snR)

{-
-- Doesn't work if the clocks aren't monoids.

-- | Compose two signal networks on the same clock in data-parallel.
--   At one tick of @cl@, both networks are stepped.
(****)
  :: Monad m
  => SN m clIn clOut a      b
  -> SN m clIn clOut     c      d
  -> SN m clIn clOut (a, c) (b, d)
Synchronous clsf1 **** Synchronous clsf2 = Synchronous $ clsf1 *** clsf2
Sequential sn11 rb1 sn12 **** Sequential sn21 rb2 sn22 = Sequential sn1 rb sn2
  where
    sn1 = sn11 **** sn21
    sn2 = sn12 **** sn22
    rb  = rb1 *-* rb2
Parallel sn11 sn12 **** Parallel sn21 sn22
  = Parallel (sn11 **** sn21) (sn12 **** sn22)
-- Note that the patterns above are the only ones that can occur.
-- This is ensured by the clock constraints in the SF constructors.
_ **** _ = error "Impossible pattern in ****"
-}

{-
firstSN :: Monad m => SN m cl cl a b -> SN m cl cl (a, c) (b, c)
firstSN (Synchronous clsf cl) = Synchronous (first clsf) cl
firstSN (Sequential sn rb cl clsf) = Sequential (firstSN sn) (firstSynchronousResamplingBuffer rb) cl (first clsf)
-- Yeah no. What if there is an intermediate slower clock?
-}


-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of @ParClock@, one of the networks is stepped,
--   dependent on which constituent clock has ticked.
--
--   Note: This is essentially an infix synonym of 'Parallel'
(||||)
  :: ( Monad m, Clock m clLa, Clock m clLb, Clock m clRa, Clock m clRb
     , Time clLa ~ Time clLb, Time clLb ~ Time clRa, Time clRa ~ Time clRb
     , GetClockProxy clLa, GetClockProxy clLb, GetClockProxy clRa, GetClockProxy clRb
     )
  => SN m           clLa                 clLb       a b
  -> SN m                clRa                 clRb  a b
  -> SN m (ParClock clLa clRa) (ParClock clLb clRb) a b
(||||) = Parallel

-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of a @ParClock@, one of the networks is stepped,
--   dependent on which constituent clock has ticked.
(++++)
  :: ( Monad m, Clock m clLa, Clock m clLb, Clock m clRa, Clock m clRb
     , Time clLa ~ Time clLb, Time clLb ~ Time clRa, Time clRa ~ Time clRb
     , GetClockProxy clLa, GetClockProxy clLb, GetClockProxy clRa, GetClockProxy clRb
     )
  => SN m           clLa                 clLb       a         b
  -> SN m                clRa                 clRb  a           b'
  -> SN m (ParClock clLa clRa) (ParClock clLb clRb) a (Either b b')
snL ++++ snR = (snL >>>^ Left) |||| (snR >>>^ Right)
