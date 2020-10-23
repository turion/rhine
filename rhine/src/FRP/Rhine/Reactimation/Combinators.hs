{- |
Combinators to create 'Rhine's (main programs) from basic components
such as 'ClSF's, clocks, 'ResamplingBuffer's and 'Schedule's.

The combinator names are often mixed of the symbols @, @*@ and @>@,
and several other symbols.
The general mnemonic for combinator names is:

* @ annotates a data processing unit such as a signal function, network or buffer
  with temporal information like a clock or a schedule.
* @*@ composes parallely.
* @>@ composes sequentially.
-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.Reactimation.Combinators where


-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.Schedule
import FRP.Rhine.SN
import FRP.Rhine.SN.Combinators
import FRP.Rhine.Type


-- * Combinators and syntactic sugar for high-level composition of signal networks.


infix 5 @@
-- | Create a synchronous 'Rhine' by combining a clocked signal function with a matching clock.
--   Synchronicity is ensured by requiring that data enters (@In cl@)
--   and leaves (@Out cl@) the system at the same as it is processed (@cl@).
(@@) :: ( cl ~ In cl
        , cl ~ Out cl )
     => ClSF m cl a b -> cl -> Rhine m cl a b
(@@) = Rhine . Synchronous

-- | A purely syntactical convenience construction
--   enabling quadruple syntax for sequential composition, as described below.
infix 2 >--
data RhineAndResamplingBuffer m cl1 inCl2 a c = forall b.
     RhineAndResamplingBuffer (Rhine m cl1 a b) (ResamplingBuffer m (Out cl1) inCl2 b c)

-- | Syntactic sugar for 'RhineAndResamplingBuffer'.
(>--) :: Rhine                    m cl1        a b
      -> ResamplingBuffer    m (Out cl1) inCl2   b c
      -> RhineAndResamplingBuffer m cl1  inCl2 a   c
(>--) = RhineAndResamplingBuffer

{- | The combinators for sequential composition allow for the following syntax:

@
rh1   :: Rhine            m      cl1           a b
rh1   =  ...

rh2   :: Rhine            m               cl2      c d
rh2   =  ...

rb    :: ResamplingBuffer m (Out cl1) (In cl2)   b c
rb    =  ...

sched :: Schedule         m      cl1      cl2
sched =  ...

rh    :: Rhine m (SequentialClock m cl1   cl2) a     d
rh    =  rh1 >-- rb -@- sched --> rh2
@
-}
infixr 1 -->
(-->) :: ( Clock m cl1
         , Clock m cl2
         , Time cl1 ~ Time cl2
         , Time (Out cl1) ~ Time cl1
         , Time (In  cl2) ~ Time cl2
         , Clock m (Out cl1), Clock m (Out cl2)
         , Clock m (In  cl1), Clock m (In  cl2)
         , In cl2 ~ inCl2
         , GetClockProxy cl1, GetClockProxy cl2
         )
      => RhineAndResamplingBuffer m cl1 inCl2  a b
      -> Rhine m                          cl2    b c
      -> Rhine m  (SequentialClock  cl1   cl2) a   c
RhineAndResamplingBuffer (Rhine sn1 cl1) rb --> (Rhine sn2 cl2)
 = Rhine (Sequential sn1 rb sn2) (SequentialClock cl1 cl2)


{- | The combinators for parallel composition allow for the following syntax:

@
rh1   :: Rhine    m                clL      a         b
rh1   =  ...

rh2   :: Rhine    m                    clR  a           c
rh2   =  ...

sched :: Schedule m                clL clR
sched =  ...

rh    :: Rhine    m (ParallelClock clL clR) a (Either b c)
rh    =  rh1 ++\@ sched \@++ rh2
@
-}
infix 3 +@+
(+@+)
  :: ( Monad m, Clock m clL, Clock m clR
     , Clock m (Out clL), Clock m (Out clR)
     , GetClockProxy clL, GetClockProxy clR
     , Time clL ~ Time (Out clL), Time clR ~ Time (Out clR)
     , Time clL ~ Time (In  clL), Time clR ~ Time (In  clR)
     , Time clL ~ Time clR
     )
       => Rhine m                clL      a         b
       -> Rhine m                    clR  a           c
       -> Rhine m (ParallelClock clL clR) a (Either b c)
Rhine sn1 clL +@+ Rhine sn2 clR
  = Rhine (sn1 ++++ sn2) (ParallelClock clL clR)

{- | The combinators for parallel composition allow for the following syntax:

@
rh1   :: Rhine    m                clL      a b
rh1   =  ...

rh2   :: Rhine    m                    clR  a b
rh2   =  ...

sched :: Schedule m                clL clR
sched =  ...

rh    :: Rhine    m (ParallelClock clL clR) a b
rh    =  rh1 ||\@ sched \@|| rh2
@
-}
infix 3 |@|
(|@|)
  :: ( Monad m, Clock m clL, Clock m clR
     , Clock m (Out clL), Clock m (Out clR)
     , GetClockProxy clL, GetClockProxy clR
     , Time clL ~ Time (Out clL), Time clR ~ Time (Out clR)
     , Time clL ~ Time (In  clL), Time clR ~ Time (In  clR)
     , Time clL ~ Time clR
     )
       => Rhine m                clL      a b
       -> Rhine m                    clR  a b
       -> Rhine m (ParallelClock clL clR) a b
Rhine sn1 clL |@| Rhine sn2 clR
  = Rhine (sn1 |||| sn2) (ParallelClock clL clR)
