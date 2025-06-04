{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExistentialQuantification #-}

{- |
The type of a complete Rhine program:
A signal network together with matching clock values.
-}
module FRP.Rhine.Type where

import Control.Arrow.Free
import Control.Monad.Schedule.Class
import Data.Automaton.Trans.Reader (runReaderS)
import Data.Profunctor
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.SN

{- |
A 'Rhine' consists of an 'SN' (signal network) together with 'Clocks' for all required clock types 'cls'.

It is a reactive program, possibly with open inputs and outputs.
If the input and output types 'a' and 'b' are both '()',
that is, the 'Rhine' is "closed",
then it is a standalone reactive program
that can be run with the function 'flow'.

Otherwise, one can start the clock and the signal network jointly as an automaton,
using 'eraseClock'.
-}
data Rhine m td cls a b = Rhine
  { clocks :: Clocks m td cls
  , sn :: SN m cls a b
  }

instance Profunctor (Rhine m td cls) where
  dimap f g Rhine {clocks, sn} =
    Rhine
      { clocks
      , sn = dimap f g sn
      }

{- |
Start the clocks and the signal network,
effectively hiding the clock types from the outside.
-}
eraseClock :: (Monad m, MonadSchedule m) => Rhine m td cls a b -> Automaton m a b
eraseClock Rhine {clocks, sn} = proc a -> do
  ti <- runClocks clocks -< ()
  runReaderS (eraseClockFreeSN sn) -< (ti, a)

{- |
Loop back data from the output to the input.

Since output and input will generally tick at different clocks,
the data needs to be resampled with a 'ResamplingBuffer'.
-}
feedbackRhine ::
  (HasClock clA cls, HasClock clB cls) =>
  ResamplingBuffer m clA clB a b ->
  Rhine m td cls (At clB b, c) (At clA a, d) ->
  Rhine m td cls c d
feedbackRhine resBuf Rhine {clocks, sn} =
  Rhine
    { clocks
    , sn = feedbackSN resBuf sn
    }

-- FIXME generalise to arbitrary number?

-- | General utility to connect to 'Rhine's by providing a function that connects the signal networks ('SN') in an arbitrary way.
wire ::
  (Clocks m td cls1 -> Clocks m td cls2 -> SN m (Append cls1 cls2) a b -> SN m (Append cls1 cls2) c d -> SN m (Append cls1 cls2) e f) ->
  Rhine m td cls1 a b ->
  Rhine m td cls2 c d ->
  Rhine m td (Append cls1 cls2) e f
wire f (Rhine cls1 sn1) (Rhine cls2 sn2) = Rhine
  { clocks = appendClocks cls1 cls2
  , sn = f cls1 cls2 (appendClocksSN cls2 sn1) (prependClocksSN cls1 sn2)
  }
