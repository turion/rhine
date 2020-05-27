{- |
Utilities to create 'ClSF's.
The fundamental effect that 'ClSF's have is
reading the time information of the clock.
It can be used for many purposes, for example digital signal processing.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module FRP.Rhine.ClSF.Util where


-- base
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category (id)
import Data.Data
import Data.Maybe (fromJust)
import Data.Monoid (Last (Last), getLast)

-- containers
import Data.Sequence

-- transformers
import Control.Monad.Trans.Reader (ask, asks)

-- dunai
import Control.Monad.Trans.MSF.Reader (readerS)
import Control.Monad.Trans.MSF.Except hiding (try, throwS)
import Data.MonadicStreamFunction.Instances.VectorSpace ()

-- simple-affine-space
import Data.VectorSpace

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ClSF.Except


-- * Read time information

-- | Read the environment variable, i.e. the 'TimeInfo'.
timeInfo :: Monad m => ClSF m cl a (TimeInfo cl)
timeInfo = constM ask

{- | Utility to apply functions to the current 'TimeInfo',
such as record selectors:
@
printAbsoluteTime :: ClSF IO cl () ()
printAbsoluteTime = timeInfoOf absolute >>> arrMCl print
@
-}
timeInfoOf :: Monad m => (TimeInfo cl -> b) -> ClSF m cl a b
timeInfoOf f = constM $ asks f

-- | Continuously return the time difference since the last tick.
sinceLastS :: Monad m => ClSF m cl a (Diff (Time cl))
sinceLastS = timeInfoOf sinceLast

-- | Continuously return the time difference since clock initialisation.
sinceInitS :: Monad m => ClSF m cl a (Diff (Time cl))
sinceInitS = timeInfoOf sinceInit

-- | Continuously return the absolute time.
absoluteS :: Monad m => ClSF m cl a (Time cl)
absoluteS = timeInfoOf absolute

-- | Continuously return the tag of the current tick.
tagS :: Monad m => ClSF m cl a (Tag cl)
tagS = timeInfoOf tag

{- |
Calculate the time passed since this 'ClSF' was instantiated.
This is _not_ the same as 'sinceInitS',
which measures the time since clock initialisation.

For example, the following gives a sawtooth signal:

@
sawtooth = safely $ do
  try $ sinceStart >>> proc time -> do
    throwOn () -< time > 1
    returnA    -< time
  safe sawtooth
@

If you replace 'sinceStart' by 'sinceInitS',
it will usually hang after one second,
since it doesn't reset after restarting the sawtooth.
-}
sinceStart :: (Monad m, Data time, Finite time, TimeDomain time) => BehaviourF m time a (Diff time)
sinceStart = absoluteS >>> proc time -> do
  startTime <- keepFirst -< time
  returnA                -< time `diffTime` startTime


-- * Useful aliases

-- TODO Is it cleverer to generalise to Arrow?
{- | Alias for 'Control.Category.>>>' (sequential composition)
with higher operator precedence, designed to work with the other operators, e.g.:

> clsf1 >-> clsf2 @@ clA ||@ sched @|| clsf3 >-> clsf4 @@ clB

The type signature specialises e.g. to

> (>->) :: Monad m => ClSF m cl a b -> ClSF m cl b c -> ClSF m cl a c
-}
infixr 6 >->
(>->) :: Category cat
      => cat a b
      -> cat   b c
      -> cat a   c
(>->) = (>>>)

-- | Alias for 'Control.Category.<<<'.
infixl 6 <-<
(<-<) :: Category cat
      => cat   b c
      -> cat a b
      -> cat a   c
(<-<) = (<<<)

{- | Output a constant value.
Specialises e.g. to this type signature:

> arr_ :: Monad m => b -> ClSF m cl a b
-}
arr_ :: Arrow a => b -> a c b
arr_ = arr . const


-- | The identity synchronous stream function.
clId :: Monad m => ClSF m cl a a
clId = Control.Category.id


-- * Basic signal processing components

-- ** Integration and differentiation

-- | The output of @integralFrom v0@ is the numerical Euler integral
--   of the input, with initial offset @v0@.
integralFrom
  :: ( Monad m, VectorSpace v s
     , Data v
     , s ~ Diff td)
  => v -> BehaviorF m td v v
integralFrom v0 = proc v -> do
  _sinceLast <- timeInfoOf sinceLast -< ()
  sumFrom v0                         -< _sinceLast *^ v

-- | Euler integration, with zero initial offset.
integral
  :: ( Monad m, VectorSpace v s
     , Data v
     , s ~ Diff td)
  => BehaviorF m td v v
integral = integralFrom zeroVector


-- | The output of @derivativeFrom v0@ is the numerical derivative of the input,
--   with a Newton difference quotient.
--   The input is initialised with @v0@.
derivativeFrom
  :: ( Monad m, VectorSpace v s
     , Data v
     , s ~ Diff td)
  => v -> BehaviorF m td v v
derivativeFrom v0 = proc v -> do
  vLast         <- iPre v0  -< v
  TimeInfo {..} <- timeInfo -< ()
  returnA                   -< (v ^-^ vLast) ^/ sinceLast

-- | Numerical derivative with input initialised to zero.
derivative
  :: ( Monad m, VectorSpace v s
     , Data v
     , s ~ Diff td)
  => BehaviorF m td v v
derivative = derivativeFrom zeroVector

-- | Like 'derivativeFrom', but uses three samples to compute the derivative.
--   Consequently, it is delayed by one sample.
threePointDerivativeFrom
  :: ( Monad m, VectorSpace v s
     , Data v
     , s ~ Diff td)
  => v -- ^ The initial position
  -> BehaviorF m td v v
threePointDerivativeFrom v0 = proc v -> do
  dv  <- derivativeFrom v0 -< v
  dv' <- iPre zeroVector   -< dv
  returnA                  -< (dv ^+^ dv') ^/ 2

-- | Like 'threePointDerivativeFrom',
--   but with the initial position initialised to 'zeroVector'.
threePointDerivative
  :: ( Monad m, VectorSpace v s
     , Data v
     , s ~ Diff td)
  => BehaviorF m td v v
threePointDerivative = threePointDerivativeFrom zeroVector

-- ** Averaging and filters

-- | A weighted moving average signal function.
--   The output is the average of the first input,
--   weighted by the second input
--   (which is assumed to be always between 0 and 1).
--   The weight is applied to the average of the last tick,
--   so a weight of 1 simply repeats the past value unchanged,
--   whereas a weight of 0 outputs the current value.
weightedAverageFrom
  :: ( Monad m, VectorSpace v s
     , s ~ Diff td
     , Data v
     )
  => v -- ^ The initial position
  -> BehaviorF m td (v, s) v
weightedAverageFrom v0 = feedback v0 $ proc ((v, weight), vAvg) -> do
  let
    vAvg' = weight *^ vAvg ^+^ (1 - weight) *^ v
  returnA -< (vAvg', vAvg')

-- | An exponential moving average, or low pass.
--   It will average out, or filter,
--   all features below a given time constant @t@.
--   (Equivalently, it filters out frequencies above @1 / (2 * pi * t)@.)
averageFrom
  :: ( Monad m, VectorSpace v s
     , Data v
     , Floating s
     , s ~ Diff td)
  => v -- ^ The initial position
  -> Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviorF m td v v
averageFrom v0 t = proc v -> do
  TimeInfo {..} <- timeInfo -< ()
  let
    weight = exp $ - (sinceLast / t)
  weightedAverageFrom v0    -< (v, weight)


-- | An average, or low pass, initialised to zero.
average
  :: ( Monad m, VectorSpace v s
     , Data v
     , Floating s
     , s ~ Diff td)
  => Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
average = averageFrom zeroVector

-- | A linearised version of 'averageFrom'.
--   It is more efficient, but only accurate
--   if the supplied time scale is much bigger
--   than the average time difference between two ticks.
averageLinFrom
  :: ( Monad m, VectorSpace v s
     , s ~ Diff td
     , Data v
     )
  => v -- ^ The initial position
  -> Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
averageLinFrom v0 t = proc v -> do
  TimeInfo {..} <- timeInfo -< ()
  let
    weight = t / (sinceLast + t)
  weightedAverageFrom v0    -< (v, weight)

-- | Linearised version of 'average'.
averageLin
  :: ( Monad m, VectorSpace v s
     , s ~ Diff td
     , Data v
     )
  => Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
averageLin = averageLinFrom zeroVector

-- *** First-order filters

-- | Alias for 'average'.
lowPass
  :: ( Monad m, VectorSpace v s
     , Data v
     , Floating s
     , s ~ Diff td)
  => Diff td
  -> BehaviourF m td v v
lowPass = average

-- | Filters out frequencies below @1 / (2 * pi * t)@.
highPass
  :: ( Monad m, VectorSpace v s
     , Data v
     , Floating s
     , s ~ Diff td)
  => Diff td -- ^ The time constant @t@
  -> BehaviourF m td v v
highPass t = clId ^-^ lowPass t

-- | Filters out frequencies other than @1 / (2 * pi * t)@.
bandPass
  :: ( Monad m, VectorSpace v s
     , Data v
     , Floating s
     , s ~ Diff td)
  => Diff td -- ^ The time constant @t@
  -> BehaviourF m td v v
bandPass t = lowPass t >>> highPass t

-- | Filters out the frequency @1 / (2 * pi * t)@.
bandStop
  :: ( Monad m, VectorSpace v s
     , Data v
     , Floating s
     , s ~ Diff td)
  => Diff td -- ^ The time constant @t@
  -> BehaviourF m td v v
bandStop t = clId ^-^ bandPass t



-- * Delays

-- | Remembers and indefinitely outputs ("holds") the first input value.
keepFirst :: (Data a, Finite a, Monad m) => ClSF m cl a a
keepFirst = safely $ do
  a <- try throwS
  safe $ arr $ const a

{-
-- | Remembers all input values that arrived within a given time window.
--   New values are appended left.
historySince
  :: (Monad m, Ord (Diff (Time cl)), TimeDomain (Time cl), Data a)
  => Diff (Time cl) -- ^ The size of the time window
  -> ClSF m cl a (Seq (TimeInfo cl, a))
historySince dTime = readerS $ accumulateWith appendValue empty
  where
    appendValue (ti, a) tias  = takeWhileL (recentlySince ti) $ (ti, a) <| tias
    recentlySince ti (ti', _) = diffTime (absolute ti) (absolute ti') < dTime

-- | Delay a signal by certain time span,
--   initialising with the first input.
delayBy
  :: ( Monad m, Ord (Diff td), TimeDomain td
     , Data a
     )
  => Diff td            -- ^ The time span to delay the signal
  -> BehaviorF m td a a
delayBy dTime = historySince dTime >>> arr (viewr >>> safeHead) >>> lastS undefined >>> arr snd
  where
    safeHead EmptyR   = Nothing
    safeHead (_ :> a) = Just a
-}
-- * Timers

-- | Throws an exception after the specified time difference,
--   outputting the time passed since the 'timer' was instantiated.
timer
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
     , Data td
     , Finite td
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
timer diff = proc _ -> do
  time <- sinceStart -< ()
  _    <- throwOn () -< time > diff
  returnA            -< time

-- | Like 'timer_', but doesn't output the remaining time at all.
timer_
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
     , Data td
     , Finite td
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a ()
timer_ diff = timer diff >>> arr (const ())

-- | Like 'timer', but divides the remaining time by the total time.
scaledTimer
  :: ( Monad m
     , TimeDomain td
     , Fractional (Diff td)
     , Ord        (Diff td)
     , Data td
     , Finite td
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
scaledTimer diff = timer diff >>> arr (/ diff)


-- * To be ported to Dunai

-- | Remembers the last 'Just' value,
--   defaulting to the given initialisation value.
lastS :: (Data a, Monad m) => a -> MSF m (Maybe a) a
lastS a = arr Last >>> mappendFrom (Last (Just a)) >>> arr (getLast >>> fromJust)
