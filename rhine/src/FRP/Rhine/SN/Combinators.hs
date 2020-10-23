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


-- | Postcompose a signal network with a pure function.
(>>>^)
  :: Monad m
  => SN m cl a b
  ->          (b -> c)
  -> SN m cl a      c
Synchronous clsf      >>>^ f = Synchronous $ clsf >>^ f
Sequential sn1 rb sn2 >>>^ f = Sequential sn1 rb     $ sn2 >>>^ f
Parallel   sn1    sn2 >>>^ f = Parallel  (sn1 >>>^ f) (sn2 >>>^ f)


-- | Precompose a signal network with a pure function.
(^>>>)
  :: Monad m
  =>        (a -> b)
  -> SN m cl      b c
  -> SN m cl a      c
f ^>>> Synchronous clsf      = Synchronous $ f ^>> clsf
f ^>>> Sequential sn1 rb sn2 = Sequential (f ^>>> sn1) rb      sn2
f ^>>> Parallel   sn1    sn2 = Parallel   (f ^>>> sn1) (f ^>>> sn2)


-- | Compose two signal networks on the same clock in data-parallel.
--   At one tick of @cl@, both networks are stepped.
(****)
  :: Monad m
  => SN m cl  a      b
  -> SN m cl     c      d
  -> SN m cl (a, c) (b, d)
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

-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of @ParClock cl1 cl2@, one of the networks is stepped,
--   dependent on which constituent clock has ticked.
--
--   Note: This is essentially an infix synonym of 'Parallel'
(||||)
  :: ( Monad m, Clock m clL, Clock m clR
     , Clock m (Out clL), Clock m (Out clR)
     , GetClockProxy clL, GetClockProxy clR
     , Time clL ~ Time clR
     , Time clL ~ Time (Out clL), Time clL ~ Time (In clL)
     , Time clR ~ Time (Out clR), Time clR ~ Time (In clR)
     )
  => SN m             clL      a b
  -> SN m                 clR  a b
  -> SN m (ParClock clL clR) a b
(||||) = Parallel

-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of @ParClock cl1 cl2@, one of the networks is stepped,
--   dependent on which constituent clock has ticked.
(++++)
  :: ( Monad m, Clock m clL, Clock m clR
     , Clock m (Out clL), Clock m (Out clR)
     , GetClockProxy clL, GetClockProxy clR
     , Time clL ~ Time clR
     , Time clL ~ Time (Out clL), Time clL ~ Time (In clL)
     , Time clR ~ Time (Out clR), Time clR ~ Time (In clR)
     )
  => SN m             clL      a         b
  -> SN m                 clR  a           c
  -> SN m (ParClock clL clR) a (Either b c)
snL ++++ snR = (snL >>>^ Left) |||| (snR >>>^ Right)
