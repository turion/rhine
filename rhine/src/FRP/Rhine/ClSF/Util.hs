{-# LANGUAGE Arrows           #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeFamilies     #-}

module FRP.Rhine.ClSF.Util where


-- base
import Control.Arrow
import Control.Category (Category)
import qualified Control.Category (id)

-- transformers
import Control.Monad.Trans.Reader (ask, asks)

-- dunai
import Data.MonadicStreamFunction (arrM_, sumFrom, delay, feedback)
import Data.VectorSpace

-- rhine
import FRP.Rhine.ClSF.Core
import FRP.Rhine.ClSF.Except


-- * Read time information

-- | Read the environment variable, i.e. the 'TimeInfo'.
timeInfo :: Monad m => ClSF m cl a (TimeInfo cl)
timeInfo = arrM_ ask

{- | Utility to apply functions to the current 'TimeInfo',
such as record selectors:
@
printAbsoluteTime :: ClSF IO cl () ()
printAbsoluteTime = timeInfoOf absolute >>> arrMCl print
@
-}
timeInfoOf :: Monad m => (TimeInfo cl -> b) -> ClSF m cl a b
timeInfoOf f = arrM_ $ asks f

-- | Continuously return the time difference since the last tick.
sinceTickS :: Monad m => ClSF m cl a (Diff (Time cl))
sinceTickS = timeInfoOf sinceTick

-- | Continuously return the time difference since clock initialisation.
sinceInitS :: Monad m => ClSF m cl a (Diff (Time cl))
sinceInitS = timeInfoOf sinceInit

-- | Continuously return the absolute time.
absoluteS :: Monad m => ClSF m cl a (Time cl)
absoluteS = timeInfoOf absolute

-- | Continuously return the tag of the current tick.
tagS :: Monad m => ClSF m cl a (Tag cl)
tagS = timeInfoOf tag

-- | Calculate the time passed since the 'ClSF' was instantiated.
timeSinceSimStart :: (Monad m, TimeDomain td) => BehaviourF m td a (Diff td)
timeSinceSimStart = proc _ -> do
  time      <- timeInfoOf absolute -< ()
  startTime <- keepFirst           -< time
  returnA                          -< time `diffTime` startTime


-- * Useful aliases

-- TODO Is it cleverer to generalise to Arrow?
{- | Alias for 'Control.Category.>>>' (sequential composition)
with higher operator precedence, designed to work with the other operators, e.g.:

> clsf1 >-> clsf2 @@ clA **@ sched @** clsf3 >-> clsf4 @@ clB

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
syncId :: Monad m => ClSF m cl a a
syncId = Control.Category.id


-- * Basic signal processing components

-- | The output of @integralFrom v0@ is the numerical Euler integral
--   of the input, with initial offset @v0@.
integralFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -> BehaviorF m td v v
integralFrom v0 = proc v -> do
  _sinceTick <- timeInfoOf sinceTick -< ()
  sumFrom v0                         -< _sinceTick *^ v

-- | Euler integration, with zero initial offset.
integral
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => BehaviorF m td v v
integral = integralFrom zeroVector


-- | The output of @derivativeFrom v0@ is the numerical derivative of the input,
--   with a Newton difference quotient.
--   The input is initialised with @v0@.
derivativeFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -> BehaviorF m td v v
derivativeFrom v0 = proc v -> do
  vLast         <- delay v0 -< v
  TimeInfo {..} <- timeInfo -< ()
  returnA                   -< (v ^-^ vLast) ^/ sinceTick

-- | Numerical derivative with input initialised to zero.
derivative
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => BehaviorF m td v v
derivative = derivativeFrom zeroVector

-- | A weighted moving average signal function.
--   The output is the average of the first input,
--   weighted by the second input
--   (which is assumed to be always between 0 and 1).
--   The weight is applied to the average of the last tick,
--   so a weight of 1 simply repeats the past value unchanged,
--   whereas a weight of 0 outputs the current value.
weightedAverageFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -- ^ The initial position
  -> BehaviorF m td (v, Groundfield v) v
weightedAverageFrom v0 = feedback v0 $ proc ((v, weight), vAvg) -> do
  let
    vAvg' = weight *^ vAvg ^+^ (1 - weight) *^ v
  returnA -< (vAvg', vAvg')

-- | An exponential moving average, or low pass.
--   It will average out, or filter,
--   all features below a given time scale.
averageFrom
  :: ( Monad m, VectorSpace v
     , Floating (Groundfield v)
     , Groundfield v ~ Diff td)
  => v -- ^ The initial position
  -> Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviorF m td v v
averageFrom v0 t = proc v -> do
  TimeInfo {..} <- timeInfo -< ()
  let
    weight = exp $ - (sinceTick / t)
  weightedAverageFrom v0    -< (v, weight)


-- | An average, or low pass, initialised to zero.
average
  :: ( Monad m, VectorSpace v
     , Floating (Groundfield v)
     , Groundfield v ~ Diff td)
  => Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
average = averageFrom zeroVector

-- | A linearised version of 'averageFrom'.
--   It is more efficient, but only accurate
--   if the supplied time scale is much bigger
--   than the average time difference between two ticks.
averageLinFrom
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => v -- ^ The initial position
  -> Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
averageLinFrom v0 t = proc v -> do
  TimeInfo {..} <- timeInfo -< ()
  let
    weight = t / (sinceTick + t)
  weightedAverageFrom v0    -< (v, weight)

-- | Linearised version of 'average'.
averageLin
  :: ( Monad m, VectorSpace v
     , Groundfield v ~ Diff td)
  => Diff td -- ^ The time scale on which the signal is averaged
  -> BehaviourF m td v v
averageLin = averageLinFrom zeroVector

-- | Remembers and indefinitely outputs ("holds") the first input value.
keepFirst :: Monad m => ClSF m cl a a
keepFirst = safely $ do
  a <- try throwS
  safe $ arr $ const a

-- * Timers

-- | Throws an exception after the specified time difference,
--   outputting the time passed since the 'timer' was instantiated.
timer
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
timer diff = proc _ -> do
  sinceSimStart <- timeSinceSimStart -< ()
  _             <- throwOn ()        -< sinceSimStart > diff
  returnA                            -< sinceSimStart

-- | Like 'timer_', but doesn't output the remaining time at all.
timer_
  :: ( Monad m
     , TimeDomain td
     , Ord (Diff td)
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
     )
  => Diff td
  -> BehaviorF (ExceptT () m) td a (Diff td)
scaledTimer diff = timer diff >>> arr (/ diff)
