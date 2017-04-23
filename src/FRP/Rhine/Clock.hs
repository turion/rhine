{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module FRP.Rhine.Clock where

import Data.MonadicStreamFunction

import FRP.Rhine.TimeDomain

class TimeDomain (TimeDomainOf c) => Clock m c where
  type TimeDomainOf c
  type Tag c
  startClock :: c -> m (MSF m () (TimeDomainOf c, Tag c), TimeDomainOf c)
  genTimeInfo :: Monad m => c -> m (MSF m a (TimeInfo c))
  genTimeInfo clock = do
    (tickingClock, initialTime) <- startClock clock
    return $ proc _ -> do
      (time, tag) <- tickingClock -< ()
      lastTime <- iPre initialTime -< time
      let sinceTick  = time `diffTime` lastTime
          sinceStart = time `diffTime` initialTime
      returnA -< TimeInfo sinceTick sinceStart time tag

data TimeInfo c = TimeInfo
  { sinceTick  :: Diff (TimeDomainOf c)
  , sinceStart :: Diff (TimeDomainOf c)
  , absolute   :: TimeDomainOf c
  , tag        :: Tag c
  }
