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
* 'cl': The clock of the whole signal network.
        It may be sequentially or parallely composed from other clocks.
* 'a': The input type. Input arrives at the rate @In cl@.
* 'b': The output type. Output arrives at the rate @Out cl@.
-}
data SN m cl a b where
  -- | A synchronous monadic stream function is the basic building block.
  --   For such an 'SN', data enters and leaves the system at the same rate as it is processed.
  Synchronous
    :: ( cl ~ In cl, cl ~ Out cl)
    => ClSF m cl a b
    -> SN   m cl a b
  -- | Two 'SN's may be sequentially composed if there is a matching 'ResamplingBuffer' between them.
  Sequential
    :: ( Clock m clab, Clock m clcd
       , Clock m (Out clab), Clock m (Out clcd)
       , Clock m (In  clab), Clock m (In  clcd)
       , GetClockProxy clab, GetClockProxy clcd
       , Time clab ~ Time clcd
       , Time clab ~ Time (Out clab)
       , Time clcd ~ Time (In  clcd)
       )
    => SN               m      clab            a b
    -> ResamplingBuffer m (Out clab) (In clcd)   b c
    -> SN               m                clcd      c d
    -> SN m (SequentialClock m clab      clcd) a     d
  -- | Two 'SN's with the same input and output data may be parallely composed.
  Parallel
    :: ( Clock m cl1, Clock m cl2
       , Clock m (Out cl1), Clock m (Out cl2)
       , GetClockProxy cl1, GetClockProxy cl2
       , Time cl1 ~ Time (Out cl1)
       , Time cl2 ~ Time (Out cl2)
       , Time cl1 ~ Time cl2
       , Time cl1 ~ Time (In cl1)
       , Time cl2 ~ Time (In cl2)
       )
    => SN m                  cl1      a b
    -> SN m                      cl2  a b
    -> SN m (ParallelClock m cl1 cl2) a b
  -- | Bypass the signal network by forwarding data in parallel through a 'ResamplingBuffer'.
  FirstResampling
    :: ( Clock m (In cl), Clock m (Out cl)
       , Time cl ~ Time (Out cl)
       , Time cl ~ Time (In cl)
       )
    => SN               m cl               a      b
    -> ResamplingBuffer m (In cl) (Out cl)    c      d
    -> SN               m cl              (a, c) (b, d)
  -- | A 'ClSF' can always be postcomposed onto an 'SN' if the clocks match on the output.
  Postcompose
    :: ( Clock m (Out cl)
       , Time cl ~ Time (Out cl)
       )
    => SN    m      cl  a b
    -> ClSF  m (Out cl)   b c
    -> SN    m      cl  a   c
  -- | A 'ClSF' can always be precomposed onto an 'SN' if the clocks match on the input.
  Precompose
    :: ( Clock m (In cl)
       , Time cl ~ Time (In cl)
       )
    => ClSF m (In cl) a b
    -> SN   m     cl    b c
    -> SN   m     cl  a   c
  -- | Data can be looped back to the beginning of an 'SN',
  --   but it must be resampled since the 'Out' and 'In' clocks are generally different.
  Feedback
    :: ( Clock m (In cl),  Clock m (Out cl)
       , Time (In cl) ~ Time cl
       , Time (Out cl) ~ Time cl
       )
    => ResBuf m (Out cl) (In cl) d c
    -> SN     m cl (a, c) (b, d)
    -> SN     m cl  a      b

instance GetClockProxy cl => ToClockProxy (SN m cl a b) where
  type Cl (SN m cl a b) = cl
