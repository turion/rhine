{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Implements pure clocks ticking at
every multiple of a fixed number of steps,
and a deterministic schedule for such clocks.
-}
module FRP.Rhine.Clock.FixedStep where

-- base
import Control.Arrow
import Control.Monad (replicateM_)
import Data.Maybe (fromMaybe)
import GHC.TypeLits

-- vector-sized
import Data.Vector.Sized (Vector, fromList)

-- automaton
import Data.Automaton (accumulateWith, constM)
import Data.Automaton.Schedule (SkipT, skip)

-- rhine
import FRP.Rhine.Clock
import FRP.Rhine.Clock.Proxy
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Collect
import FRP.Rhine.ResamplingBuffer.Util

{- | A pure (side effect free) clock with fixed step size,
   i.e. ticking at multiples of 'n'.
   The tick rate is in the type signature,
   which prevents composition of signals at different rates.
-}
data FixedStep (n :: Nat) where
  FixedStep :: (KnownNat n) => FixedStep n -- TODO Does the constraint bring any benefit?

-- | Extract the type-level natural number as an integer.
stepsize :: FixedStep n -> Integer
stepsize fixedStep@FixedStep = natVal fixedStep

instance (Monad m) => Clock (SkipT m) (FixedStep n) where
  type Time (FixedStep n) = Integer
  type Tag (FixedStep n) = ()
  initClock cl =
    let step = stepsize cl
     in pure
          ( constM (replicateM_ (fromIntegral step - 1) skip) -- skip n - 1 steps and return a value on the n-th step
              >>> arr (const step)
              >>> accumulateWith (+) 0
              >>> arr (,())
          , 0
          )
  {-# INLINE initClock #-}

instance GetClockProxy (FixedStep n)

-- | A singleton clock that counts the ticks.
type Count = FixedStep 1

{- | Resample into a 'FixedStep' clock that ticks @n@ times slower,
  by collecting all values into a vector.
-}
downsampleFixedStep ::
  (KnownNat n, Monad m) =>
  ResamplingBuffer m (FixedStep k) (FixedStep (n * k)) a (Vector n a)
downsampleFixedStep = collect >>-^ arr (fromList >>> assumeSize)
  where
    assumeSize = fromMaybe $ error "downsampleFixedStep: Internal error. Please report this as a bug: https://github.com/turion/rhine/issues"
