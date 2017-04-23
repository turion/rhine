{-# LANGUAGE GADTs #-}
module FRP.Rhine.SF where


import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingPoint
import FRP.Rhine.Schedule
import FRP.Rhine.SyncSF

data SF m cl a b where
  Synchronous :: ( Clock m cl
                 , cl ~ Leftmost cl
                 , cl ~ Rightmost cl
                 )
              => SyncSF m cl a b
              -> SF m cl a b
  Resampling  :: ( Clock m clab
                 , Clock m clcd
                 , TimeDomainOf clab ~ TimeDomainOf clcd
                 , TimeDomainOf clab ~ TimeDomainOf (Rightmost clab)
                 , TimeDomainOf clcd ~ TimeDomainOf (Leftmost  clcd)
                 )
              => SF m clab a b
              -> ResamplingBuffer m (Rightmost clab) (Leftmost clcd) b c
              -> SF m clcd c d
              -> SF m (CombinedClock m clab clcd) a d
