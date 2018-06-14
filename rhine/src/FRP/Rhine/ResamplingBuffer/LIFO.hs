{-# LANGUAGE RecordWildCards #-}
module FRP.Rhine.ResamplingBuffer.LIFO where

-- base
import Prelude hiding (length, take)

-- containers
import Data.Sequence

-- rhine
import FRP.Rhine.ResamplingBuffer
import FRP.Rhine.ResamplingBuffer.Timeless

-- * LIFO (last-in-first-out) buffers

-- | An unbounded LIFO buffer.
--   If the buffer is empty, it will return 'Nothing'.
lifo :: Monad m => ResamplingBuffer m cl1 cl2 a (Maybe a)
lifo = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as   = case viewl as of
      EmptyL   -> return (Nothing, empty)
      a :< as' -> return (Just a , as'  )

-- |  A bounded LIFO buffer that forgets the oldest values when the size is above a given threshold.
--   If the buffer is empty, it will return 'Nothing'.
boundedLifo :: Monad m => Int -> ResamplingBuffer m cl1 cl2 a (Maybe a)
boundedLifo threshold = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a
      | (length as) >= threshold = return $ a <| (take threshold as)
      | otherwise = return $ a <| as
    amGet as = case viewl as of
      EmptyL     -> return (Nothing, empty)
      a :< as'  -> return (Just a , as'  )


-- | An unbounded LIFO buffer that also returns its current size.
lifoWatch :: Monad m => ResamplingBuffer m cl1 cl2 a (Maybe a, Int)
lifoWatch = timelessResamplingBuffer AsyncMealy {..} empty
  where
    amPut as a = return $ a <| as
    amGet as   = case viewl as of
      EmptyL   -> return ((Nothing, 0         ), empty)
      a :< as' -> return ((Just a , length as'), as'  )
