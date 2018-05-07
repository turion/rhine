{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module FRP.Rhine.Clock.Cycle (CycleClock (CycleClock)) where

-- base
import Control.Monad (forever)
import GHC.TypeLits (Nat, KnownNat, natVal)

-- dunai
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Maybe (listToMaybeS)
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import Control.Monad.Schedule

-- * The 'CycleClock'

-- | A clock whose tick lengths cycle through
--   a (nonempty) list of type-level natural numbers.
--   E.g. @CycleClock '[1, 2]@ ticks at times 1, 3, 4, 5, 7, 8, etc.
--
--   The waiting side effect is formal, in 'ScheduleT'.
--   You can use e.g. 'runScheduleIO' to produce an actual delay.
data CycleClock (v :: [Nat]) where
  CycleClock :: CycleClock (n : ns)

instance (Monad m, NonemptyNatList v)
      => Clock (ScheduleT Integer m) (CycleClock v) where
  type TimeDomainOf (CycleClock v) = Integer
  type Tag          (CycleClock v) = ()
  startClock cl = return
    ( cycleS (theList cl) >>> withSideEffect wait >>> sumS &&& arr (const ())
    , 0
    )

-- * Type-level trickery to extract the type value from the singleton

data HeadClProxy (n :: Nat) where
  HeadClProxy :: CycleClock (n : ns) -> HeadClProxy n

headCl :: KnownNat n => CycleClock (n : ns) -> Integer
headCl cl = natVal $ HeadClProxy cl

tailCl :: CycleClock (n1 : n2 : ns) -> CycleClock (n2 : ns)
tailCl CycleClock = CycleClock

class NonemptyNatList (v :: [Nat]) where
  theList :: CycleClock v -> [Integer]

instance KnownNat n => NonemptyNatList '[n] where
  theList cl = [headCl cl]

instance (KnownNat n1, KnownNat n2, NonemptyNatList (n2 : ns))
      => NonemptyNatList (n1 : n2 : ns) where
  theList cl = headCl cl : theList (tailCl cl)


-- * Utilities

-- TODO Port back to dunai when naming issues are resolved
-- | Repeatedly outputs the values of a given list, in order.
cycleS :: Monad m => [a] -> MSF m arbitrary a
cycleS = safely . forever . try . maybeToExceptS . listToMaybeS

{-
-- TODO Port back to dunai when naming issues are resolved
delayList :: [a] -> MSF a a
delayList [] = id
delayList (a : as) = delayList as >>> delay a
-}
