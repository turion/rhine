{-# LANGUAGE RecordWildCards #-}

{- |
A buffer keeping the last value, or zero-order hold.
-}
module FRP.Rhine.ResamplingBuffer.KeepLast where

-- automaton
import Data.Stream.Result (Result (..))

-- rhine
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Timeless

{- | Always keeps the last input value,
   or in case of no input an initialisation value.
   If @cl2@ approximates continuity,
   this behaves like a zero-order hold.
-}
keepLast :: (Monad m) => a -> ResamplingBuffer m cl1 cl2 a a
keepLast = timelessResamplingBuffer AsyncMealy {..}
  where
    amGet a = return $! Result a a
    amPut _ = return
