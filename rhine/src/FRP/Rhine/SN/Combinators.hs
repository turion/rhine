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
Injection sn1 rb sn2 >>>^ f = Injection sn1 rb $ sn2 >>>^ f
SeqInjection sn1 sn2 >>>^ f = SeqInjection sn1 $ sn2 >>>^ f
Postcompose sn clsf >>>^ f = Postcompose sn $ clsf >>^ f
Precompose clsf sn >>>^ f = Precompose clsf $ sn >>>^ f
Feedback buf sn >>>^ f = Feedback buf $ sn >>>^ first f

-- | Precompose a signal network with a pure function.
(^>>>)
  :: Monad m
  =>        (a -> b)
  -> SN m cl      b c
  -> SN m cl a      c
f ^>>> Synchronous clsf      = Synchronous $ f ^>> clsf
f ^>>> Sequential sn1 rb sn2 = Sequential (f ^>>> sn1) rb      sn2
f ^>>> Parallel   sn1    sn2 = Parallel   (f ^>>> sn1) (f ^>>> sn2)
f ^>>> Injection sn1 rb sn2 = Injection sn1 rb $ (first f) ^>>> sn2
f ^>>> SeqInjection sn1 sn2 = SeqInjection (f ^>>> sn1) sn2
f ^>>> Postcompose sn clsf = Postcompose (f ^>>> sn) clsf
f ^>>> Precompose clsf sn = Precompose (f ^>> clsf) sn
f ^>>> Feedback buf sn = Feedback buf $ first f ^>>> sn

-- | Postcompose a signal network with a 'ClSF'.
(>--^)
  :: ( Clock m (Out cl)
     , Time cl ~ Time (Out cl)
     )
  => SN    m      cl  a b
  -> ClSF  m (Out cl)   b c
  -> SN    m      cl  a   c
(>--^) = Postcompose

-- | Precompose a signal network with a 'ClSF'.
(^-->)
  :: ( Clock m (In cl)
     , Time cl ~ Time (In cl)
     )
  => ClSF m (In cl) a b
  -> SN   m     cl    b c
  -> SN   m     cl  a   c
(^-->) = Precompose

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

Injection sn23 rb34 sn145 **** Injection sn78 rb89 sn690
  = Injection (fixUnit ^>>> (sn23 **** sn78)) (rb34 *-* rb89) (fixInput ^>>> (sn145 **** sn690))
  where
    fixUnit :: () -> ((), ())
    fixUnit () = ((), ())
    fixInput :: ((a,c), (b,d)) -> ((a,b), (c,d))
    fixInput ((a,c), (b,d)) = ((a,b), (c,d))

SeqInjection sn11 sn12 **** SeqInjection sn21 sn22
  = SeqInjection (sn11 **** sn21) (sn12 **** sn22)

Precompose clsf sn1 **** sn2 = Precompose (first clsf) $ sn1 **** sn2
sn1 **** Precompose clsf sn2 = Precompose (second clsf) $ sn1 **** sn2
Postcompose sn1 clsf **** sn2 = Postcompose (sn1 **** sn2) (first clsf)
sn1 **** Postcompose sn2 clsf = Postcompose (sn1 **** sn2) (second clsf)

Feedback buf sn1 **** sn2 = Feedback buf $ (\((a, c), c1) -> ((a, c1), c)) ^>>> (sn1 **** sn2) >>>^ (\((b, d1), d) -> ((b, d), d1))
sn1 **** Feedback buf sn2 = Feedback buf $ (\((a, c), c1) -> (a, (c, c1))) ^>>> (sn1 **** sn2) >>>^ (\(b, (d, d1)) -> ((b, d), d1))

-- Note that the patterns above are the only ones that can occur.
-- This is ensured by the clock constraints in the SF constructors.
Synchronous _ **** Parallel _ _ = error "Impossible pattern: Synchronous _ **** Parallel _ _"
Parallel _ _ **** Synchronous _ = error "Impossible pattern: Parallel _ _ **** Synchronous _"
Synchronous _ **** Sequential {} = error "Impossible pattern: Synchronous _ **** Sequential {}"
Sequential {} **** Synchronous _ = error "Impossible pattern: Sequential {} **** Synchronous _"
Synchronous _ **** Injection _ _ _ = error "Impossible pattern: Synchronous _ **** Injection _ _ _"
Injection _ _ _ **** Synchronous _ = error "Impossible pattern: Injection _ _ _ **** Synchronous _"
Synchronous _ **** SeqInjection _ _ = error "Impossible pattern: Synchronous _ **** SeqInjection _"
SeqInjection _ _ **** Synchronous _ = error "Impossible pattern: SeqInjection _ _ **** Synchronous _"
Injection _ _ _ **** SeqInjection _ _ = error "Impossible pattern: Injection _ _ _ **** SeqInjection _ _"
SeqInjection _ _ **** Injection _ _ _ = error "Impossible pattern: SeqInjection _ _ **** Injection _ _ _"


-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of @ParClock m cl1 cl2@, one of the networks is stepped,
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
  -> SN m (ParClock m clL clR) a b
(||||) = Parallel

-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of @ParClock m cl1 cl2@, one of the networks is stepped,
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
  -> SN m (ParClock m clL clR) a (Either b c)
snL ++++ snR = (snL >>>^ Left) |||| (snR >>>^ Right)
