{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module FRP.Rhine.SN where


-- rhine
import FRP.Rhine.Clock
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
* 'a': The input type. Input arrives at the rate @Leftmost cl@.
* 'b': The output type. Output arrives at the rate @Rightmost cl@.
-}
data SN m cl a b where
  -- | A synchronous monadic stream function is the basic building block.
  --   For such an 'SN', data enters and leaves the system at the same rate as it is processed.
  Synchronous
    :: ( cl ~ Leftmost cl, cl ~ Rightmost cl)
    => ClSF m cl a b
    -> SN   m cl a b
  -- | Two 'SN's may be sequentially composed if there is a matching 'ResamplingBuffer' between them.
  Sequential
    :: ( Clock m clab, Clock m clcd
       , Time clab ~ Time clcd
       , Time clab ~ Time (Rightmost clab)
       , Time clcd ~ Time (Leftmost  clcd)
       )
    => SN               m            clab                  a b
    -> ResamplingBuffer m (Rightmost clab) (Leftmost clcd)   b c
    -> SN               m                            clcd      c d
    -> SN m (SequentialClock m       clab            clcd) a     d
  -- | Two 'SN's with the same input and output data may be parallely composed.
  Parallel
    :: ( Clock m cl1, Clock m cl2
       , Time cl1 ~ Time (Rightmost cl1)
       , Time cl2 ~ Time (Rightmost cl2)
       , Time cl1 ~ Time cl2
       , Time cl1 ~ Time (Leftmost cl1)
       , Time cl2 ~ Time (Leftmost cl2)
       )
    => SN m cl1 a b
    -> SN m cl2 a b
    -> SN m (ParallelClock m cl1 cl2) a b
