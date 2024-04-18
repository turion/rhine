{-# LANGUAGE RecordWildCards #-}

{- |
Different implementations of LIFO buffers.
-}
module FRP.Rhine.ResamplingBuffer.LIFO where

-- base
import Prelude hiding (length, take)

-- containers
import Data.Sequence

-- rhine
import Data.Stream.Result (Result (..))
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Timeless

-- * LIFO (last-in-first-out) buffers

{- | An unbounded LIFO buffer.
   If the buffer is empty, it will return 'Nothing'.
-}
lifoUnbounded :: (Monad m) => ResamplingBuffer m cl1 cl2 a (Maybe a)
lifoUnbounded = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as = case viewl as of
      EmptyL -> return $! Result empty Nothing
      a :< as' -> return $! Result as' (Just a)

{- |  A bounded LIFO buffer that forgets the oldest values when the size is above a given threshold.
   If the buffer is empty, it will return 'Nothing'.
-}
lifoBounded :: (Monad m) => Int -> ResamplingBuffer m cl1 cl2 a (Maybe a)
lifoBounded threshold = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ take threshold $ a <| as
    amGet as = case viewl as of
      EmptyL -> return $! Result empty Nothing
      a :< as' -> return $! Result as' (Just a)

-- | An unbounded LIFO buffer that also returns its current size.
lifoWatch :: (Monad m) => ResamplingBuffer m cl1 cl2 a (Maybe a, Int)
lifoWatch = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as = case viewl as of
      EmptyL -> return $! Result empty (Nothing, 0)
      a :< as' -> return $! Result as' (Just a, length as')
