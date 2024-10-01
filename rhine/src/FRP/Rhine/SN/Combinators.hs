{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{- |
Combinators for composing signal networks sequentially and parallely.
-}
module FRP.Rhine.SN.Combinators where

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ResamplingBuffer.Util
import FRP.Rhine.Schedule
import FRP.Rhine.Reactimation.ClockErasure
import Data.Functor ((<&>))

{- FOURMOLU_DISABLE -}
-- | Postcompose a signal network with a pure function.
(>>>^)
  :: Monad m
  => SN m cl a b
  ->          (b -> c)
  -> SN m cl a      c
SN {getSN} >>>^ f = SN $ getSN <&> (>>> arr (fmap f))

-- | Precompose a signal network with a pure function.
(^>>>)
  :: Monad m
  =>        (a -> b)
  -> SN m cl      b c
  -> SN m cl a      c
f ^>>> SN {getSN} = SN $ getSN <&> (arr (fmap (fmap f)) >>>)

-- | Postcompose a signal network with a 'ClSF'.
(>--^)
  :: ( GetClockProxy cl , Clock m (Out cl)
     , Time cl ~ Time (Out cl)
     , Monad m
     )
  => SN    m      cl  a b
  -> ClSF  m (Out cl)   b c
  -> SN    m      cl  a   c
(>--^) = postcompose

-- | Precompose a signal network with a 'ClSF'.
(^-->)
  :: ( Clock m (In cl), GetClockProxy cl, Monad m
     , Time cl ~ Time (In cl)
     )
  => ClSF m (In cl) a b
  -> SN   m     cl    b c
  -> SN   m     cl  a   c
(^-->) = precompose

-- | Compose two signal networks on the same clock in data-parallel.
--   At one tick of @cl@, both networks are stepped.
(****)
  :: Monad m
  => SN m cl  a      b
  -> SN m cl     c      d
  -> SN m cl (a, c) (b, d)
SN sn1 **** SN sn2 = SN $ do
  sn1' <- sn1
  sn2' <- sn2
  pure $ arr (\(time, tag, mac) -> ((time, tag, fst <$> mac), (time, tag, snd <$> mac))) >>> (sn1' *** sn2') >>> arr (\(mb, md) -> (,) <$> mb <*> md)

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
(||||) = parallel

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
