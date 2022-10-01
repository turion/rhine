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


-- | A point at which sequential asynchronous composition
--   ("resampling") of signal networks can happen.
data ResamplingPoint m cla clb a b = ResamplingPoint
  (ResamplingBuffer m (Out cla) (In clb) a b)
  (Schedule m cla clb)
-- TODO Make a record out of it?
-- TODO This is aesthetically displeasing.
--      For the buffer, the associativity doesn't matter, but for the Schedule,
--      we sometimes need to specify particular brackets in order for it to work.
--      This is confusing.
--      There would be a workaround if there were pullbacks of schedules...

-- | Syntactic sugar for 'ResamplingPoint'.
infix 8 -@-
(-@-) :: ResamplingBuffer m (Out cl1) (In cl2) a b
      -> Schedule         m      cl1      cl2
      -> ResamplingPoint  m      cl1      cl2  a b
(-@-) = ResamplingPoint

-- | A purely syntactical convenience construction
--   enabling quadruple syntax for sequential composition, as described below.
infix 2 >--
data RhineAndResamplingPoint m cl1 cl2 a c = forall b.
     RhineAndResamplingPoint (Rhine m cl1 a b) (ResamplingPoint m cl1 cl2 b c)

-- | Syntactic sugar for 'RhineAndResamplingPoint'.
(>--) :: Rhine                   m cl1     a b
      -> ResamplingPoint         m cl1 cl2   b c
      -> RhineAndResamplingPoint m cl1 cl2 a   c
(>--) = RhineAndResamplingPoint

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
         , GetClockProxy cl1, GetClockProxy cl2
         )
      => RhineAndResamplingPoint   m cl1 cl2  a b
      -> Rhine m                         cl2    b c
      -> Rhine m  (SequentialClock m cl1 cl2) a   c
RhineAndResamplingPoint (Rhine sn1 cl1) (ResamplingPoint rb cc) --> (Rhine sn2 cl2)
 = Rhine (Sequential sn1 rb sn2) (SequentialClock cl1 cl2 cc)

-- | A purely syntactical convenience construction
--   allowing for ternary syntax for parallel composition, described below.
data RhineParallelAndSchedule m clL clR a b
  = RhineParallelAndSchedule (Rhine m clL a b) (Schedule m clL clR)

-- | Syntactic sugar for 'RhineParallelAndSchedule'.
infix 4 ++@
(++@)
  :: Rhine                    m clL     a b
  -> Schedule                 m clL clR
  -> RhineParallelAndSchedule m clL clR a b
(++@) = RhineParallelAndSchedule

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
infix 3 @++
(@++)
  :: ( Monad m, Clock m clL, Clock m clR
     , Clock m (Out clL), Clock m (Out clR)
     , GetClockProxy clL, GetClockProxy clR
     , Time clL ~ Time (Out clL), Time clR ~ Time (Out clR)
     , Time clL ~ Time (In  clL), Time clR ~ Time (In  clR)
     , Time clL ~ Time clR
     )
       => RhineParallelAndSchedule m clL clR  a b
       -> Rhine                    m     clR  a c
       -> Rhine m (ParallelClock   m clL clR) a (Either b c)
RhineParallelAndSchedule (Rhine sn1 clL) schedule @++ (Rhine sn2 clR)
  = Rhine (sn1 ++++ sn2) (ParallelClock clL clR schedule)

-- | Further syntactic sugar for 'RhineParallelAndSchedule'.
infix 4 ||@
(||@)
  :: Rhine                    m clL     a b
  -> Schedule                 m clL clR
  -> RhineParallelAndSchedule m clL clR a b
(||@) = RhineParallelAndSchedule

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
infix 3 @||
(@||)
  :: ( Monad m, Clock m clL, Clock m clR
     , Clock m (Out clL), Clock m (Out clR)
     , GetClockProxy clL, GetClockProxy clR
     , Time clL ~ Time (Out clL), Time clR ~ Time (Out clR)
     , Time clL ~ Time (In  clL), Time clR ~ Time (In  clR)
     , Time clL ~ Time clR
     )
       => RhineParallelAndSchedule m clL clR  a b
       -> Rhine                    m     clR  a b
       -> Rhine m (ParallelClock   m clL clR) a b
RhineParallelAndSchedule (Rhine sn1 clL) schedule @|| (Rhine sn2 clR)
  = Rhine (sn1 |||| sn2) (ParallelClock clL clR schedule)


-- | Postcompose a 'Rhine' with a pure function.
(@>>^)
  :: Monad m
  => Rhine m cl a b
  ->             (b -> c)
  -> Rhine m cl a      c
Rhine sn cl @>>^ f = Rhine (sn >>>^ f) cl

-- | Precompose a 'Rhine' with a pure function.
(^>>@)
  :: Monad m
  =>           (a -> b)
  -> Rhine m cl      b c
  -> Rhine m cl a      c
f ^>>@ Rhine sn cl = Rhine (f ^>>> sn) cl

-- | Postcompose a 'Rhine' with a 'ClSF'.
(@>-^)
  :: ( Clock m (Out cl)
     , Time cl ~ Time (Out cl)
     )
  => Rhine m      cl  a b
  -> ClSF  m (Out cl)   b c
  -> Rhine m      cl  a   c
Rhine sn cl @>-^ clsf = Rhine (sn >--^ clsf) cl

-- | Precompose a 'Rhine' with a 'ClSF'.
(^->@)
  :: ( Clock m (In cl)
     , Time cl ~ Time (In cl)
     )
  => ClSF  m (In cl) a b
  -> Rhine m     cl    b c
  -> Rhine m     cl  a   c
clsf ^->@ Rhine sn cl = Rhine (clsf ^--> sn) cl

  
-- | Syntactic sugar for 'RhineAndResamplingPoint' with '()' specified as the input.
infix 2 >=-
(>=-) :: Rhine m cl1 () a
      -> ResamplingPoint m cl1 cl2 a b
      -> RhineAndResamplingPoint m cl1 cl2 () b
(>=-) = RhineAndResamplingPoint

{- | The combinators for injection composition allow for the following syntax:

@
rh1   :: Rhine            m      cl1           () b
rh1   =  ...

rh2   :: Rhine            m               cl2  (a, c) d
rh2   =  ...

rb    :: ResamplingBuffer m (Out cl1) (In cl2) b c
rb    =  ...

sched :: Schedule         m       cl1     cl2
sched =  ...

rh    :: Rhine m (InjectionClock m cl1    cl2) a d
rh    =  rh1 >=- rb -@- sched -=> rh2
@
-}
infix 1 -=>
(-=>) :: ( Clock m cl1, Clock m cl2
         , Clock m (Out cl1)
         , Clock m (In cl2)
         , GetClockProxy cl1, GetClockProxy cl2
         , Time cl1 ~ Time cl2
         , Time cl1 ~ Time (Out cl1)
         , Time cl2 ~ Time (In cl2)
         )
      => RhineAndResamplingPoint m cl1 cl2 () b
      -> Rhine m                       cl2 (a, b) c
      -> Rhine m (InjectionClock m cl1 cl2) a c
RhineAndResamplingPoint (Rhine sn1 cl1) (ResamplingPoint rb cc) -=> (Rhine sn2 cl2)
  = Rhine (Injection sn1 rb sn2) (InjectionClock cl1 cl2 cc)

-- | Two schedules necessary to combine two 'InjectionClock's
--   to allow the convenient construction of a 'SeqInjection'.
data InjScheduling m clL clR cl1 =
     InjScheduling (Schedule m clL clR) (Schedule m (ParallelClock m clL clR) cl1)

-- | Syntactic sugar for 'InjScheduling'.
infix 8 =@=
(=@=) :: Schedule      m clL clR
      -> Schedule      m (ParallelClock m clL clR) cl1
      -> InjScheduling m clL clR cl1
(=@=) = InjScheduling

-- | A purely syntactical convenience construction
--   enabling quadruple syntax for sequential injection composition, as described below.
data RhineAndInjScheduling m clL clR cl1 a b =
     RhineAndInjScheduling (Rhine m (InjectionClock m clL cl1) a b) (InjScheduling m clL clR cl1)

-- | Syntactic sugar for 'RhineAndInjScheduling'.
infix 2 >>=-
(>>=-) :: Rhine m (InjectionClock m clL cl1) a b
       -> InjScheduling m clL clR cl1
       -> RhineAndInjScheduling m clL clR cl1 a b
(>>=-) = RhineAndInjScheduling

{- | The combinators for sequential injection composition allow for the following syntax:

@
rh1    :: Rhine m (InjectionClock m clL cl1) a b
rh1    =  ...

rh2    :: Rhine m (InjectionClock m clR cl1) b c
rh2    =  ...

sched1 :: Schedule m clL clR
sched1 =  ...

sched2 :: Schedule m (ParallelClock m clL clR) cl1
sched2 =  ...

rh     :: Rhine (InjectionClock m (ParallelClock m clL clR) cl1) a c
rh     =  rh1 >>=- sched1 =@= sched2 -=>> rh2
@
-}
infix 1 -=>>
(-=>>) :: ( Clock m cl1, Clock m clL, Clock m clR
          , GetClockProxy cl1
          , GetClockProxy clL
          , GetClockProxy clR
          , Time cl1 ~ Time clL
          , Time clL ~ Time clR
          )
       => RhineAndInjScheduling m clL clR cl1 a b
       -> Rhine m (InjectionClock m clR cl1) b c
       -> Rhine m (InjectionClock m (ParallelClock m clL clR) cl1) a c
RhineAndInjScheduling (Rhine sn1 (InjectionClock clL cl1 _)) (InjScheduling lr cc)
  -=>> (Rhine sn2 (InjectionClock clR _ _))
  = Rhine (SeqInjection sn1 sn2) (InjectionClock (ParallelClock clL clR lr) cl1 cc)
