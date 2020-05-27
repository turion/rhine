{-# LANGUAGE RecordWildCards #-}

{- |
Different implementations of LIFO buffers.
-}
module FRP.Rhine.ResamplingBuffer.LIFO where

-- base
import Data.Data
import Prelude hiding (length, take)

-- containers
import Data.Sequence

-- rhine
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Timeless

-- * LIFO (last-in-first-out) buffers

{- | An unbounded LIFO buffer.
  If the buffer is empty, it will return 'Nothing'.
-}
lifoUnbounded :: (Monad m, Data a) => ResamplingBuffer m cl1 cl2 a (Maybe a)
lifoUnbounded = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as = case viewl as of
      EmptyL -> return (Nothing, empty)
      a :< as' -> return (Just a, as')

{- |  A bounded LIFO buffer that forgets the oldest values when the size is above a given threshold.
  If the buffer is empty, it will return 'Nothing'.
-}
lifoBounded :: (Monad m, Data a) => Int -> ResamplingBuffer m cl1 cl2 a (Maybe a)
lifoBounded threshold = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ take threshold $ a <| as
    amGet as = case viewl as of
      EmptyL -> return (Nothing, empty)
      a :< as' -> return (Just a, as')

-- | An unbounded LIFO buffer that also returns its current size.
lifoWatch :: (Monad m, Data a) => ResamplingBuffer m cl1 cl2 a (Maybe a, Int)
lifoWatch = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as = case viewl as of
      EmptyL -> return ((Nothing, 0), empty)
      a :< as' -> return ((Just a, length as'), as')
