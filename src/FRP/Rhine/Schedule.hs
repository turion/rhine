{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FRP.Rhine.Schedule where

import Data.MonadicStreamFunction

import FRP.Rhine.Clock

data Schedule m c1 c2 = (TimeDomainOf c1 ~ TimeDomainOf c2)
  => Schedule { startSchedule :: c1 -> c2 -> m (MSF m () (TimeDomainOf c1, Either (Tag c1) (Tag c2)), TimeDomainOf c1) }
-- TODO Does it make sense to have constraints in this constructor?
-- Rather use GADTs?
-- TODO Break the long line

data CombinedClock m c1 c2 where
  CombinedClock :: TimeDomainOf c1 ~ TimeDomainOf c2
                => c1 -> c2 -> Schedule m c1 c2
                -> CombinedClock m c1 c2
-- TODO Do we need that constraint here? Can we do without GADTs?

instance (Monad m, Clock m c1, Clock m c2)
      => Clock m (CombinedClock m c1 c2) where
  type TimeDomainOf (CombinedClock m c1 c2)      = TimeDomainOf c1
  type Tag (CombinedClock m c1 c2)               = Either (Tag c1) (Tag c2)
  startClock (CombinedClock c1 c2 (Schedule schedule)) = schedule c1 c2

type family Leftmost c where
  Leftmost (CombinedClock m c1 c2) = Leftmost c1
  Leftmost c                       = c

type family Rightmost c where
  Rightmost (CombinedClock m c1 c2) = Rightmost c2
  Rightmost c                       = c
