{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
The type of a complete Rhine program:
A signal network together with a matching clock value.
-}
module FRP.Rhine.Type where

-- dunai
import Data.MonadicStreamFunction

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Reactimation.ClockErasure
import FRP.Rhine.ResamplingBuffer (ResamplingBuffer)
import FRP.Rhine.SN
import FRP.Rhine.Schedule (In, Out)

{- |
A 'Rhine' consists of a 'SN' together with a clock of matching type 'cl'.

It is a reactive program, possibly with open inputs and outputs.
If the input and output types 'a' and 'b' are both '()',
that is, the 'Rhine' is "closed",
then it is a standalone reactive program
that can be run with the function 'flow'.

Otherwise, one can start the clock and the signal network jointly as a monadic stream function,
using 'eraseClock'.
-}
data Rhine m cl a b = Rhine
  { sn :: SN m cl a b
  , clock :: cl
  }

instance GetClockProxy cl => ToClockProxy (Rhine m cl a b) where
  type Cl (Rhine m cl a b) = cl

{- |
Start the clock and the signal network,
effectively hiding the clock type from the outside.

Since the caller will not know when the clock @'In' cl@ ticks,
the input 'a' has to be given at all times, even those when it doesn't tick.
-}
eraseClock ::
  (Monad m, Clock m cl, GetClockProxy cl) =>
  Rhine m cl a b ->
  m (MSF m a (Maybe b))
eraseClock Rhine {..} = do
  (runningClock, initTime) <- initClock clock
  -- Run the main loop
  return $ proc a -> do
    (time, tag) <- runningClock -< ()
    eraseClockSN initTime sn -< (time, tag, a <$ inTag (toClockProxy sn) tag)

{- |
Loop back data from the output to the input.

Since output and input will generally tick at different clocks,
the data needs to be resampled.
-}
feedbackRhine ::
  ( Clock m (In cl)
  , Clock m (Out cl)
  , Time (In cl) ~ Time cl
  , Time (Out cl) ~ Time cl
  ) =>
  ResamplingBuffer m (Out cl) (In cl) d c ->
  Rhine m cl (a, c) (b, d) ->
  Rhine m cl a b
feedbackRhine buf Rhine {..} =
  Rhine
    { sn = Feedback buf sn
    , clock
    }
