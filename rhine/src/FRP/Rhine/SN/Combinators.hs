{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

{- |
Combinators for composing signal networks sequentially and parallely.
-}
module FRP.Rhine.SN.Combinators where

-- base
import Data.Functor ((<&>))

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.SN
import FRP.Rhine.Schedule
import Data.Profunctor (Profunctor(..))

{- FOURMOLU_DISABLE -}
{- | Postcompose a signal network with a pure function.

This is just a flipped alias for 'rmap'.
-}
(>>>^)
  :: Monad m
  => SN m cls a b
  ->           (b -> c)
  -> SN m cls a      c
(>>>^) = flip rmap

{- | Precompose a signal network with a pure function.

This is just an alias for 'lmap'.
-}
(^>>>)
  :: Monad m
  =>         (a -> b)
  -> SN m cls      b c
  -> SN m cls a      c
(^>>>) = lmap

-- | Postcompose a signal network with a 'ClSF'.
(>--^)
  :: (HasClock clbc cls, Monad m, Clock m clbc)
  => SN    m        cls  a b
  -> ClSF  m   clbc        b c
  -> SN    m        cls  a  (At clbc c)
sn >--^ clsf = sn >>>^ Present >>> synchronous clsf

-- | Precompose a signal network with a 'ClSF'.
(^-->)
  :: (HasClock clab cls, Clock m clab)
  => ClSF m         clab a b
  -> SN   m cls (At clab   b) c
  -> SN   m cls (At clab a)   c
clsf ^--> sn = synchronous clsf >>> sn

{- | Compose two signal networks on the same clocks in data-parallel.

At one tick of any of @cls@, both networks are stepped.

This is just an alias for '***'.
-}
(****)
  :: Monad m
  => SN m cls  a      b
  -> SN m cls     c      d
  -> SN m cls (a, c) (b, d)
(****) = (***)


-- FIXME These type signatures are in general not implementable because Append is not injective. Maybe there is some way around this?

-- -- | Compose two signal networks on different clocks in clock-parallel.
-- --   At one tick of any of @cls1@ or @cls2@, either or both of the networks are stepped,
-- --   dependent on which clock has ticked.
-- (||||)
--   :: SN m         cls1       a b
--   -> SN m              cls2  a b
--   -> SN m (Append cls1 cls2) a b
-- (||||) = parallel

-- | Compose two signal networks on different clocks in clock-parallel.
--   At one tick of @ParClock cl1 cl2@, one of the networks is stepped,
--   dependent on which constituent clock has ticked.
-- (++++)
--   :: ( Monad m, Clock m clL, Clock m clR
--      , Clock m (Out clL), Clock m (Out clR)
--      , GetClockProxy clL, GetClockProxy clR
--      , Time clL ~ Time clR
--      , Time clL ~ Time (Out clL), Time clL ~ Time (In clL)
--      , Time clR ~ Time (Out clR), Time clR ~ Time (In clR)
--      )
--   => SN m             cls1      a         b
--   -> SN m                 cls2  a           c
--   -> SN m (Append cls1 cls2)    a (Either b c)
-- snL ++++ snR = (snL >>>^ Left) |||| (snR >>>^ Right)
