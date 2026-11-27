{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{- |
Periodic clocks are defined by a stream of ticks with periodic time differences.
They model subclocks of a fixed reference clock.
The time differences are supplied at the type level.
-}
module FRP.Rhine.Clock.Periodic (Periodic (Periodic)) where

-- base
import Control.Arrow

-- automaton
import Data.Automaton (
  accumulateWith,
  arrM,
  concatS,
 )
import Data.Automaton.Schedule.Trans (ScheduleT, wait)
import Data.List.NonEmpty hiding (unfold)

-- time-domain
import Data.TimeDomain (Seconds (..))

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import GHC.TypeLits (KnownNat, Nat, natVal)

-- * The 'Periodic' clock

{- | A clock whose tick lengths cycle through
  a (nonempty) list of type-level natural numbers.
  E.g. @Periodic '[1, 2]@ ticks at times 1, 3, 4, 5, 7, 8, etc.

  The waiting side effect is formal, in 'ScheduleT'.
  You can use e.g. 'runScheduleIO' to produce an actual delay.
-}
data Periodic (v :: [Nat]) where
  Periodic :: Periodic (n : ns)

instance
  (Monad m, NonemptyNatList v, time ~ Seconds Integer) =>
  Clock (ScheduleT time m) (Periodic v)
  where
  type Time (Periodic v) = Seconds Integer
  type Tag (Periodic v) = ()
  runClock = concatS (arr (toList . theList)) >>> accumulateWith (+) 0 &&& arrM (wait . fromIntegral)
  {-# INLINE runClock #-}

instance GetClockProxy (Periodic v)

-- * Type-level trickery to extract the type value from the singleton

data HeadClProxy (n :: Nat) where
  HeadClProxy :: Periodic (n : ns) -> HeadClProxy n

headCl :: (KnownNat n) => Periodic (n : ns) -> Integer
headCl cl = natVal $ HeadClProxy cl

tailCl :: Periodic (n1 : n2 : ns) -> Periodic (n2 : ns)
tailCl Periodic = Periodic

class NonemptyNatList (v :: [Nat]) where
  theList :: Periodic v -> NonEmpty (Seconds Integer)

instance (KnownNat n) => NonemptyNatList '[n] where
  theList cl = Seconds (headCl cl) :| []

instance
  (KnownNat n1, KnownNat n2, NonemptyNatList (n2 : ns)) =>
  NonemptyNatList (n1 : n2 : ns)
  where
  theList cl = Seconds (headCl cl) <| theList (tailCl cl)
