{- |
Asynchronous signal networks are combinations of clocked signal functions ('ClSF's)
and matching 'ResamplingBuffer's,
all satisfying the appropriate clock type constraints.

This module defines the 'SN' type,
combinators are found in a submodule.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module FRP.Rhine.SN where


-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule


{- | An 'SN' is a side-effectful asynchronous /__s__ignal __n__etwork/,
where input, data processing (including side effects) and output
need not happen at the same time.

The type parameters are:

* 'm': The monad in which side effects take place.
* 'clIn': The clock at which data enters the signal network.
        It may be parallely composed from other clocks.
* 'clOut': The clock at which data leaves the signal network.
        It may be parallely composed from other clocks.
* 'a': The input type. Input arrives at the rate @clIn@.
* 'b': The output type. Output arrives at the rate @clOut@.
-}
data SN m clIn clOut a b where
  -- | A synchronous monadic stream function is the basic building block.
  --   For such an 'SN', data enters and leaves the system at the same rate as it is processed.
  Synchronous
    :: ( Clock m cl, cl ~ In cl, cl ~ Out cl
       , GetClockProxy cl
       )
    => ClSF m cl    a b
    ->        cl
    -> SN   m cl cl a b
  -- | To an 'SN' we can append a clock and synchronous stream function,
  --   if there is a matching 'ResamplingBuffer' between them.
  Sequential
    :: ( Clock m cla, Clock m clb, Clock m clcd
       , GetClockProxy cla, GetClockProxy clb, GetClockProxy clcd
       , Time cla ~ Time clb, Time cla ~ Time clcd
       )
    => SN               m cla clb      a b
    -> ResamplingBuffer m     clb clcd   b c
    ->                            clcd
    -> ClSF             m         clcd     c d
    -> SN               m cla     clcd a     d
  -- | Two 'SN's with the same input and output data may be parallely composed.
  Parallel
    :: ( Clock m clLa, Clock m clRa, Clock m clLb, Clock m clRb
       , GetClockProxy clLa, GetClockProxy clRa, GetClockProxy clLb, GetClockProxy clRb
       , Time clLa ~ Time clRa, Time clLa ~ Time clLb, Time clLa ~ Time clRb
       )
    => SN m                clLa                      clLb       a b
    -> SN m                     clRa                      clRb  a b
    -> SN m (ParallelClock clLa clRa) (ParallelClock clLb clRb) a b
