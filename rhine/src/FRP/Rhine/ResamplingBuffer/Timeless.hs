{-# LANGUAGE RecordWildCards #-}

{- |
Resampling buffers from asynchronous Mealy machines.
These are used in many other modules implementing 'ResamplingBuffer's.
-}
module FRP.Rhine.ResamplingBuffer.Timeless where

import FRP.Rhine.ResamplingBuffer

{- | An asynchronous, effectful Mealy machine description.
   (Input and output do not happen simultaneously.)
   It can be used to create 'ResamplingBuffer's.
-}
{- FOURMOLU_DISABLE -}
data AsyncMealy m s a b = AsyncMealy
  { amPut :: s -> a -> m     s
  -- ^ Given the previous state and an input value, return the new state.
  , amGet :: s      -> m (b, s)
  -- ^ Given the previous state, return an output value and a new state.
  }
{- FOURMOLU_ENABLE -}

{- | A resampling buffer that is unaware of the time information of the clock,
   and thus clock-polymorphic.
   It is built from an asynchronous Mealy machine description.
   Whenever 'get' is called on @timelessResamplingBuffer machine s@,
   the method 'amGet' is called on @machine@ with state @s@,
   discarding the time stamp. Analogously for 'put'.
-}
timelessResamplingBuffer ::
  (Monad m) =>
  AsyncMealy m s a b -> -- The asynchronous Mealy machine from which the buffer is built

  -- | The initial state
  s ->
  ResamplingBuffer m cl1 cl2 a b
timelessResamplingBuffer AsyncMealy {..} = go
  where
    go s =
      let
        put _ a = go <$> amPut s a
        get _ = do
          (b, s') <- amGet s
          return (b, go s')
       in
        ResamplingBuffer {..}

-- | A resampling buffer that only accepts and emits units.
trivialResamplingBuffer :: (Monad m) => ResamplingBuffer m cl1 cl2 () ()
trivialResamplingBuffer =
  timelessResamplingBuffer
    AsyncMealy
      { amPut = const (const (return ()))
      , amGet = const (return ((), ()))
      }
    ()
