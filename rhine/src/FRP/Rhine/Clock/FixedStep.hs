{- |
Implements pure clocks ticking at
every multiple of a fixed number of steps,
and a deterministic schedule for such clocks.
-}

{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module FRP.Rhine.Clock.FixedStep where


-- base
import Data.Maybe (fromMaybe)
import GHC.TypeLits

-- fixed-vector
import Data.Vector.Sized (Vector, fromList)

-- dunai
import Data.MonadicStreamFunction.Async (concatS)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.Util
import FRP.Rhine.Schedule

-- | A pure (side effect free) clock with fixed step size,
--   i.e. ticking at multiples of 'n'.
--   The tick rate is in the type signature,
--   which prevents composition of signals at different rates.
data FixedStep (n :: Nat) where
  FixedStep :: KnownNat n => FixedStep n -- TODO Does the constraint bring any benefit?

-- | Extract the type-level natural number as an integer.
stepsize :: FixedStep n -> Integer
stepsize fixedStep@FixedStep = natVal fixedStep

instance Monad m => Clock m (FixedStep n) where
  type Time (FixedStep n) = Integer
  type Tag  (FixedStep n) = ()
  initClock cl = return
    ( count >>> arr (* stepsize cl)
      &&& arr (const ())
    , 0
    )

-- | A singleton clock that counts the ticks.
type Count = FixedStep 1

-- | Two 'FixedStep' clocks can always be scheduled without side effects.
scheduleFixedStep
  :: Monad m
  => Schedule m (FixedStep n1) (FixedStep n2)
scheduleFixedStep = Schedule f where
  f cl1 cl2 = return (msf, 0)
    where
      n1 = stepsize cl1
      n2 = stepsize cl2
      msf = concatS $ proc _ -> do
        k <- arr (+1) <<< count -< ()
        returnA                 -< [ (k, Left  ()) | k `mod` n1 == 0 ]
                                ++ [ (k, Right ()) | k `mod` n2 == 0 ]

-- TODO The problem is that the schedule doesn't give a guarantee where in the n ticks of the first clock the second clock will tick.
-- For this to work, it has to be the last.
-- With scheduleFixedStep, this works,
-- but the user might implement an incorrect schedule.
downsampleFixedStep
  :: (KnownNat n, Monad m)
  => ResamplingBuffer m (FixedStep k) (FixedStep (n * k)) a (Vector n a)
downsampleFixedStep = collect >>-^ arr (fromList >>> assumeSize)
  where
    assumeSize = fromMaybe $ error $ unwords
      [ "You are using an incorrectly implemented schedule"
      , "for two FixedStep clocks."
      , "Use a correct schedule like downsampleFixedStep."
      ]
