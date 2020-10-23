{- |
The type of a complete Rhine program:
A signal network together with a matching clock value.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.Type where

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Reactimation.ClockErasure
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.SN
import Control.Monad.Schedule.Class

{- |
A 'Rhine' a reactive program.

Possibly, it has open inputs and outputs.
If the input and output types 'a' and 'b' are both '()',
that is, the 'Rhine' is "closed",
then it is a standalone reactive program
that can be run with the function 'flow'.

Otherwise, one can start the clock and the signal network jointly as a monadic stream function,
using 'eraseClock'.
-}
type Rhine m cla clb a b = SN m cla clb a b


{- |
Start the clock and the signal network,
effectively hiding the clock type from the outside.

Since the caller will not know when the clock @'In' cl@ ticks,
the input 'a' has to be given at all times, even those when it doesn't tick.
-}
eraseClock
  :: (Monad m, MonadSchedule m)
  => Rhine  m cla clb a        b
  -> m (MSF m         a (Maybe b))
eraseClock = eraseClockSN
