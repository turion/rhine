{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

{- | General mnemonic for combinators:

* @ annotates a data processing unit such as a signal function or a buffer
  with temporal information like a clock or a schedule.
* @*@ composes parallely.
* @>@ composes sequentially.
-}
module FRP.Rhine.SF.Combinators where


-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Reactimation
import FRP.Rhine.Schedule
import FRP.Rhine.SF
import FRP.Rhine.ClSF


-- * Combinators and syntactic sugar for high-level composition of signal functions.


infix 5 @@
-- | Create a synchronous 'Rhine' by combining a synchronous SF with a matching clock.
--   Synchronicity is ensured by requiring that data enters (@Leftmost cl@)
--   and leaves (@Rightmost cl@) the system at the same as it is processed (@cl@).
(@@) :: ( cl ~ Leftmost cl
        , cl ~ Rightmost cl )
     => ClSF m cl a b -> cl -> Rhine m cl a b
(@@) = Rhine . Synchronous


-- | A point at which sequential asynchronous composition
--   ("resampling") of signal functions can happen.
data ResamplingPoint m cla clb a b = ResamplingPoint
  (ResamplingBuffer m (Rightmost cla) (Leftmost clb) a b)
  (Schedule m cla clb)
-- TODO Make a record out of it?
-- TODO This is aesthetically displeasing.
--      For the buffer, the associativity doesn't matter, but for the Schedule,
--      we sometimes need to specify particular brackets in order for it to work.
--      This is confusing.
--      There would be a workaround if there were pullbacks of schedules...

-- | Syntactic sugar for 'ResamplingPoint'.
infix 8 -@-
(-@-) :: ResamplingBuffer m (Rightmost cl1) (Leftmost cl2) a b
      -> Schedule m cl1 cl2
      -> ResamplingPoint m cl1 cl2 a b
(-@-) = ResamplingPoint

-- | A purely syntactical convenience construction
--   enabling quadruple syntax for sequential composition, as described below.
infix 2 >--
data RhineAndResamplingPoint m cl1 cl2 a c = forall b.
     RhineAndResamplingPoint (Rhine m cl1 a b) (ResamplingPoint m cl1 cl2 b c)

-- | Syntactic sugar for 'RhineAndResamplingPoint'.
(>--) :: Rhine m cl1 a b -> ResamplingPoint m cl1 cl2 b c -> RhineAndResamplingPoint m cl1 cl2 a c
(>--) = RhineAndResamplingPoint

{- | The combinators for sequential composition allow for the following syntax:

@
rh1   :: Rhine            m            cl1                 a b
rh1   =  ...

rh2   :: Rhine            m                           cl2      c d
rh2   =  ...

rb    :: ResamplingBuffer m (Rightmost cl1) (Leftmost cl2)   b c
rb    =  ...

sched :: Schedule         m            cl1            cl2
sched =  ...

rh    :: Rhine            m (SequentialClock cl1 cl2)      a     d
rh    =  rh1 >-- rb -@- sched --> rh2
@
-}
infixr 1 -->
(-->) :: ( Clock m cl1
         , Clock m cl2
         , Time cl1 ~ Time cl2
         , Time (Rightmost cl1) ~ Time cl1
         , Time (Leftmost  cl2) ~ Time cl2
         , Clock m (Rightmost cl1)
         , Clock m (Leftmost  cl2)
         )
      => RhineAndResamplingPoint   m cl1 cl2  a b
      -> Rhine m                         cl2    b c
      -> Rhine m  (SequentialClock m cl1 cl2) a   c
RhineAndResamplingPoint (Rhine sf1 cl1) (ResamplingPoint rb cc) --> (Rhine sf2 cl2)
 = Rhine (Sequential sf1 rb sf2) (SequentialClock cl1 cl2 cc)

-- | A purely syntactical convenience construction
--   allowing for ternary syntax for parallel composition, described below.
data RhineParallelAndSchedule m cl1 cl2 a b = RhineParallelAndSchedule (Rhine m cl1 a b) (Schedule m cl1 cl2)

-- | Syntactic sugar for 'RhineParallelAndSchedule'.
infix 4 **@
(**@)
  :: Rhine                    m cl1     a b
  -> Schedule                 m cl1 cl2
  -> RhineParallelAndSchedule m cl1 cl2 a b
(**@) = RhineParallelAndSchedule

{- | The combinators for parallel composition allow for the following syntax:

@
rh1   :: Rhine    m                cl1      a b
rh1   =  ...

rh2   :: Rhine    m                    cl2  a b
rh2   =  ...

sched :: Schedule m                cl1 cl2
sched =  ...

rh    :: Rhine    m (ParallelClock cl1 cl2) a b
rh    =  rh1 **\@ sched \@** rh2
@
-}
infix 3 @**
(@**) :: ( Clock m cl1
          , Clock m cl2
          , Time cl1 ~ Time (Rightmost cl1)
          , Time cl2 ~ Time (Rightmost cl2)
          , Time cl1 ~ Time (Leftmost cl1)
          , Time cl2 ~ Time (Leftmost cl2)
          , Time cl1 ~ Time cl2
          )
       => RhineParallelAndSchedule m cl1 cl2 a b
       -> Rhine m cl2 a b
       -> Rhine m (ParallelClock m cl1 cl2) a b
RhineParallelAndSchedule (Rhine sf1 cl1) schedule @** (Rhine sf2 cl2)
  = Rhine (Parallel sf1 sf2) (ParallelClock cl1 cl2 schedule)
