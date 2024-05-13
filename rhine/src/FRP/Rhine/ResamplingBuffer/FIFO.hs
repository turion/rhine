{-# LANGUAGE RecordWildCards #-}

{- |
Different implementations of FIFO buffers.
-}
module FRP.Rhine.ResamplingBuffer.FIFO where

-- base
import Prelude hiding (length, take)

-- containers
import Data.Sequence

-- automaton
import Data.Stream.Result (Result (..))

-- rhine
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Timeless

-- * FIFO (first-in-first-out) buffers

{- | An unbounded FIFO buffer.
   If the buffer is empty, it will return 'Nothing'.
-}
fifoUnbounded :: (Monad m) => ResamplingBuffer m cl1 cl2 a (Maybe a)
fifoUnbounded = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as = case viewr as of
      EmptyR -> return $! Result empty Nothing
      as' :> a -> return $! Result as' (Just a)

{- |  A bounded FIFO buffer that forgets the oldest values when the size is above a given threshold.
    If the buffer is empty, it will return 'Nothing'.
-}
fifoBounded :: (Monad m) => Int -> ResamplingBuffer m cl1 cl2 a (Maybe a)
fifoBounded threshold = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ take threshold $ a <| as
    amGet as = case viewr as of
      EmptyR -> return $! Result empty Nothing
      as' :> a -> return $! Result as' (Just a)

-- | An unbounded FIFO buffer that also returns its current size.
fifoWatch :: (Monad m) => ResamplingBuffer m cl1 cl2 a (Maybe a, Int)
fifoWatch = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as = case viewr as of
      EmptyR -> return $! Result empty (Nothing, 0)
      as' :> a -> return $! Result as' (Just a, length as')
