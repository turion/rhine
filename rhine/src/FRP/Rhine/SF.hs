{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}
module FRP.Rhine.SF where


-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule
import FRP.Rhine.ClSF


{- | 'SF' is an abbreviation for "signal function".
It represents a side-effectful asynchronous /__s__ignal __f__unction/, or signal network,
where input, data processing (including side effects) and output
need not happen at the same time.

The type parameters are:

* 'm': The monad in which side effects take place.
* 'cl': The clock of the whole signal network.
        It may be sequentially or parallely composed from other clocks.
* 'a': The input type. Input arrives at the rate @Leftmost cl@.
* 'b': The output type. Output arrives at the rate @Rightmost cl@.
-}
data SF m cl a b where
  -- | A synchronous monadic stream function is the basic building block.
  --   For such an 'SF', data enters and leaves the system at the same rate as it is processed.
  Synchronous
    :: ( cl ~ Leftmost cl, cl ~ Rightmost cl)
    => ClSF m cl a b
    -> SF     m cl a b
  -- | Two 'SF's may be sequentially composed if there is a matching 'ResamplingBuffer' between them.
  Sequential
    :: ( Clock m clab, Clock m clcd
       , Time clab ~ Time clcd
       , Time clab ~ Time (Rightmost clab)
       , Time clcd ~ Time (Leftmost  clcd)
       )
    => SF               m            clab                  a b
    -> ResamplingBuffer m (Rightmost clab) (Leftmost clcd)   b c
    -> SF               m                            clcd      c d
    -> SF m (SequentialClock m       clab            clcd) a     d
  -- | Two 'SF's with the same input and output data may be parallely composed.
  Parallel
    :: ( Clock m cl1, Clock m cl2
       , Time cl1 ~ Time (Rightmost cl1)
       , Time cl2 ~ Time (Rightmost cl2)
       , Time cl1 ~ Time cl2
       , Time cl1 ~ Time (Leftmost cl1)
       , Time cl2 ~ Time (Leftmost cl2)
       )
    => SF m cl1 a b
    -> SF m cl2 a b
    -> SF m (ParallelClock m cl1 cl2) a b
