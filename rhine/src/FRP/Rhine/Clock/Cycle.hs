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
module FRP.Rhine.Clock.Cycle where

-- base
import Control.Monad (forever)
import GHC.TypeLits (Nat, KnownNat, natVal)

-- dunai
import Control.Monad.Trans.MSF.Except
import Control.Monad.Trans.MSF.Maybe (listToMaybeS)
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine
import Control.Monad.Schedule

{-

-- TODO Port back to dunai
delayList :: [a] -> MSF a a
delayList [] = id
delayList (a : as) = delayList as >>> delay a

-}

-- TODO Port back to dunai
cycleS :: Monad m => [a] -> MSF m arbitrary a
cycleS = safely . forever . try . maybeToExceptS . listToMaybeS

data CycleClock (v :: [Nat]) where
  CycleClock :: CycleClock (n : ns)

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

instance (Monad m, NonemptyNatList v)
      => Clock (ScheduleT Integer m) (CycleClock v) where
  type TimeDomainOf (CycleClock v) = Integer
  type Tag          (CycleClock v) = ()
  startClock cl = return
    ( cycleS (theList cl) >>> withSideEffect wait >>> sumS &&& arr (const ())
    , 0
    )
