{-# LANGUAGE GADTs #-}

{- |
Run closed 'Rhine's (which are signal functions together with matching clocks)
as main loops.
-}
module FRP.Rhine.Reactimation where

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.Reactimation.Combinators
import FRP.Rhine.Schedule
import FRP.Rhine.Type

-- * Running a Rhine

{- |
Takes a closed 'Rhine' (with trivial input and output),
and runs it indefinitely.
This is typically the main loop.

All input has to be created, and all output has to be consumed
by means of side effects in a monad 'm'.

Basic usage (synchronous case):

@
sensor :: ClSF MyMonad MyClock () a
sensor = constMCl produceData

processing :: ClSF MyMonad MyClock a b
processing = ...

actuator :: ClSF MyMonad MyClock b ()
actuator = arrMCl consumeData

mainSF :: ClSF MyMonad MyClock () ()
mainSF = sensor >-> processing >-> actuator

main :: MyMonad ()
main = flow $ mainSF @@ clock
@
-}

-- TODO Can we chuck the constraints into Clock m cl?
flow ::
  ( Monad m
  , Clock m cl
  , GetClockProxy cl
  , Time cl ~ Time (In cl)
  , Time cl ~ Time (Out cl)
  ) =>
  Rhine m cl () () ->
  m void
flow rhine = reactimate $ eraseClock rhine >>> arr (const ())
{-# INLINE flow #-}

{- | Like 'flow', but with the type signature specialized to @m ()@.

This is sometimes useful when dealing with ambiguous types.
-}
flow_ ::
  ( Monad m
  , Clock m cl
  , GetClockProxy cl
  , Time cl ~ Time (In cl)
  , Time cl ~ Time (Out cl)
  ) =>
  Rhine m cl () () ->
  m ()
flow_ = flow

{- | Run a synchronous 'ClSF' with its clock as a main loop,
   similar to Yampa's, or Dunai's, 'reactimate'.
-}
reactimateCl ::
  ( Monad m
  , Clock m cl
  , GetClockProxy cl
  , cl ~ In cl
  , cl ~ Out cl
  ) =>
  cl ->
  ClSF m cl () () ->
  m ()
reactimateCl cl clsf = flow $ clsf @@ cl
{-# INLINE reactimateCl #-}
